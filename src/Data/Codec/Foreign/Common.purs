module Data.Codec.Foreign.Common 
  ( module Data.Codec.Foreign.Common
  , module Data.Codec.Foreign
  ) where

import Prelude hiding (map)

import Data.Array as Array
import Data.Codec (decode, encode, (~))
import Data.Codec.Foreign (Codec(..), Codec', ForeignCodec, FPropCodec, ForeignDecodingError(..), array, boolean,  codePoint, codec, codec', decode, encode, fix, hoist, identity, index, indexedArray, int, farray, fobject, foreign_, null, number, object, printForeignDecodingError, prismaticCodec, prop, record, recordProp, recordPropOptional, string, void, (<~<), (>~>), (~))
-- char,named, coercible, 
import Data.Codec.Foreign.Sum (taggedSum)
import Data.Either (Either(..))
import Data.Functor as F
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | A codec for `Maybe` values.
-- |
-- | NOTE: This is not suitable to en/decode null values. If you need these kinds of codecs,
-- | look into `Data.Codec.Argonaut.Compat`
maybe ∷ ∀ a. ForeignCodec a → ForeignCodec (Maybe a)
maybe codec = taggedSum "Maybe" printTag parseTag dec enc
  where
  printTag = case _ of
    false → "Nothing"
    true → "Just"
  parseTag = case _ of
    "Nothing" → Just false
    "Just" → Just true
    _ → Nothing
  dec = case _ of
    false → Left Nothing
    true → Right (F.map Just <<< decode codec)
  enc = case _ of
    Nothing → Tuple false Nothing
    Just a → Tuple true (Just (encode codec a))

-- | A codec for `Tuple` values.
-- |
-- | Encodes as a two-element array in JSON.
tuple ∷ ∀ a b. ForeignCodec a → ForeignCodec b → ForeignCodec (Tuple a b)
tuple codecA codecB = indexedArray "Tuple" $
  Tuple
    <$> fst ~ index 0 codecA
    <*> snd ~ index 1 codecB

-- | A codec for `Either` values.
either ∷ ∀ a b. ForeignCodec a → ForeignCodec b → ForeignCodec (Either a b)
either codecA codecB = taggedSum "Either" printTag parseTag dec enc
  where
  printTag = case _ of
    true → "Left"
    false → "Right"
  parseTag = case _ of
    "Left" → Just true
    "Right" → Just false
    _ → Nothing
  dec = case _ of
    true → Right (F.map Left <<< decode codecA)
    false → Right (F.map Right <<< decode codecB)
  enc = case _ of
    Left a → Tuple true (Just (encode codecA a))
    Right b → Tuple false (Just (encode codecB b))

-- | A codec for `List` values.
-- |
-- | Encodes as an array in JSON.
list ∷ ∀ a. ForeignCodec a → ForeignCodec (List a)
list = dimap Array.fromFoldable List.fromFoldable <<< array

-- | A codec for `Map` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
map ∷ ∀ a b. Ord a ⇒ ForeignCodec a → ForeignCodec b → ForeignCodec (Map a b)
map codecA = dimap Map.toUnfoldable Map.fromFoldable <<< array <<< tuple codecA

-- | A codec for `StrMap` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
foreignObject ∷ ∀ a. ForeignCodec a → ForeignCodec (Object a)
foreignObject =
  dimap Object.toUnfoldable Object.fromFoldable <<< array <<< tuple string
