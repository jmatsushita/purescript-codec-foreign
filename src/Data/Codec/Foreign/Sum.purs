module Data.Codec.Foreign.Sum where

import Prelude

import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Foreign (ForeignCodec, ForeignDecodingError(..), fobject, foreign_, prop, string)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Object as FO
import Foreign.Object.ST as FOST

-- | A helper for defining Foreign codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum ∷ ∀ a. (a → String) → (String → Maybe a) → ForeignCodec a
enumSum printTag parseTag =
  Codec.codec
    (\f → do
      value <- Codec.decode string f
      case parseTag value of 
        Just a → Right a
        Nothing → Left (UnexpectedTagValue value))
    (Codec.encode string <<< printTag)

-- | A helper for defining Foreign codecs for sum types. To ensure exhaustivity
-- | there needs to be a mapping to and from a tag type for the type to be
-- | encoded.
-- |
-- | - The first argument is the name of the type being decoded, for error
-- |   message purposes.
-- | - The second argument maps a tag value to a string to use in the encoding.
-- | - The third argument maps a string back to a tag value during decoding.
-- | - The fourth argument returns either a constant value or a decoder function
-- |   based on a tag value.
-- | - The fifth argument returns a tag value and optional encoded value to
-- |   store for a constructor of the sum.
taggedSum
  ∷ ∀ tag a
  . String
  → (tag → String)
  → (String → Maybe tag)
  → (tag → Either a (Foreign → Either ForeignDecodingError a))
  → (a → Tuple tag (Maybe Foreign))
  → ForeignCodec a
taggedSum name printTag parseTag f g = Codec.codec decodeCase encodeCase
  where
  decodeCase ∷ Foreign -> Either ForeignDecodingError a
  decodeCase v = lmap (Named name) do
    obj ← Codec.decode fobject v
    tag ← Codec.decode (prop "tag" string) obj
    case parseTag tag of
      Nothing → Left (AtKey "tag" (UnexpectedTagValue tag))
      Just t →
        case f t of
          Left a → pure a
          Right decoder → do
            value ← Codec.decode (prop "value" foreign_) obj
            lmap (AtKey "value") (decoder value)

  encodeCase ∷ a -> Foreign
  encodeCase a = case g a of
    Tuple tag value → Codec.encode fobject $
        FO.runST do
          obj ← FOST.new
          _ ← FOST.poke "tag" (Codec.encode string (printTag tag)) obj
          maybe (pure obj) (\v → FOST.poke "value" v obj) value
