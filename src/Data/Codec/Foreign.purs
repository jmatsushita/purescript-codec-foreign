module Data.Codec.Foreign 
  ( ForeignCodec
  , ForeignDecodingError(..)
  , printForeignDecodingError
  , foreign_
  , null
  , boolean
  , number
  , int
  , string
  , codePoint
  -- , char
  , farray
  , fobject
  , void
  , array
  , FIndexedCodec
  , indexedArray
  , index
  , FPropCodec
  , object
  , prop
  , record
  , recordProp
  , recordPropOptional
  , fix
  -- , named
  -- , coercible
  , prismaticCodec
  , module Codec
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError, withExcept)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (Codec(..), Codec', codec, codec', decode, encode, hoist, identity, (<~<), (>~>), (~)) as Codec
import Data.Codec as C
import Data.Either (Either(..), either, note)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List.NonEmpty (foldMap1)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..), renderForeignError)
import Foreign as F
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Record.Unsafe as Record
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

data ForeignDecodingError
  = ForeignDecodingErrors F.MultipleErrors
  | InvalidCodePoint
  | VoidError
  | AtIndex Int ForeignDecodingError
  | AtKey String ForeignDecodingError
  | Named String ForeignDecodingError
  | MissingValueAtIndex Int
  | MissingValueAtKey String
  | UnexpectedTagValue String
  | UnexpectedValue String

derive instance Eq ForeignDecodingError
derive instance Generic ForeignDecodingError _

instance Show ForeignDecodingError where
  show = printForeignDecodingError

-- | Prints a `ForeignDecodeError` as a somewhat readable error message.
printForeignDecodingError ∷ ForeignDecodingError → String
printForeignDecodingError err =
  "An error occurred while decoding a Foreign value:\n" <> go err
  where
  go = case _ of
    ForeignDecodingErrors es → foldMap1 (append "\t" <<< renderForeignError) es
    InvalidCodePoint → "\tInvalid code point"
    VoidError → "\tVoid error"
    AtIndex ix inner → "\tAt array index " <> show ix <> ":\n" <> go inner
    AtKey key inner → "\tAt object key " <> key <> ":\n" <> go inner
    Named name inner → "\tUnder '" <> name <> "':\n" <> go inner
    MissingValueAtIndex ix → "\tNo value was found at index " <> show ix
    MissingValueAtKey key → "\tNo value was found at key " <> show key
    UnexpectedTagValue v → "\tUnexpected tag value: " <> show v
    UnexpectedValue v → "\tUnexpected value: " <> show v

-- | Codec type for `Foreign` values.
type ForeignCodec a = C.Codec' (Either ForeignDecodingError) Foreign a

-- | The "identity codec" for `Foreign` values.
foreign_ ∷ ForeignCodec Foreign
foreign_ = C.codec' pure identity

-- | A codec for `null` values in `Foreign`.
null ∷ ForeignCodec Foreign
null = C.hoist (withExcept ForeignDecodingErrors >>> runExcept) $
  C.codec' (\r → F.readNull r >>= f r) (const _null)
  where
  f ∷ Foreign → Maybe Foreign → F Foreign
  f x = maybe (throwError $ pure $ TypeMismatch "null" (F.typeOf x)) pure

-- | A codec for `Boolean` values in `Foreign`.
boolean ∷ ForeignCodec Boolean
boolean = decodec' (decodingError <<< F.readBoolean)

-- | A codec for `Number` values in `Foreign`.
number ∷ ForeignCodec Number
number = decodec' (decodingError <<< F.readNumber)

-- | A codec for `Int` values in `Foreign`.
int ∷ ForeignCodec Int
int = decodec' (decodingError <<< F.readInt)

-- | A codec for `String` values in `Foreign`.
string ∷ ForeignCodec String
string = decodec' (decodingError <<< F.readString)

-- | A codec for `Codepoint` values in `Foreign`.
codePoint ∷ ForeignCodec CodePoint
codePoint = C.codec'
  (decodeCodePoint <=< (decodingError <<< F.readString))
  (F.unsafeToForeign <<< S.singleton)
  where
  decodeCodePoint ∷ String → _ CodePoint
  decodeCodePoint = maybe codePointError pure <<< S.codePointAt 0
    where
    codePointError = throwError InvalidCodePoint

-- | A codec for `Void` values.
void ∷ ForeignCodec Void
void = C.codec' (const $ throwError VoidError) absurd

-- | A codec for `Array Foreign` values in `Foreign`. This does not decode
-- | the values of the array, for that use `array` for a general array decoder,
-- | or `indexedArray` with `index` to decode fixed length array encodings.
farray ∷ ForeignCodec (Array Foreign)
farray = decodec' (decodingError <<< F.readArray)

-- | A codec for `Object` values in `Foreign`.
fobject ∷ ForeignCodec (FO.Object Foreign)
fobject = decodec' $ decodingError <<< \f → case F.typeOf f of
  "object" → pure $ unsafeCoerce f
  ty → throwError $ pure $ TypeMismatch ty "object"

-- | A codec for arbitrary length `Array`s where every item in the array
-- | shares the same type.
-- |
-- | ``` purescript
-- | import Codec.Foreign as CF
-- |
-- | codecIntArray ∷ CF.ForeignCodec (Array Int)
-- | codecIntArray = CF.array CF.int
-- | ```
array ∷ ∀ a. ForeignCodec a → ForeignCodec (Array a)
array codec 
  = C.codec' 
    ( \f → traverseWithIndex ( \ix a → lmap (AtIndex ix) (C.decode codec a)) =<< C.decode farray f)
    (\a → F.unsafeToForeign (map (C.encode codec) a))

-- | Codec type for specifically indexed `Array` elements.
type FIndexedCodec a =
  C.Codec (Either ForeignDecodingError) (Array Foreign) (List Foreign) a a

-- | A codec for types that are encoded as an array with a specific layout.
-- |
-- | For example, if we'd like to encode a `Person` as a 2-element array, like
-- | `["Rashida", 37]`, we could write the following codec:
-- |
-- | ```purescript
-- | import Codec.Foreign ((~))
-- | import Codec.Foreign as CF
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CF.ForeignCodec Person
-- | codecPerson = CF.indexedArray "Test Object" $
-- |   { name: _, age: _ }
-- |     <$> _.name ~ CF.index 0 CF.string
-- |     <*> _.age ~ CF.index 1 CF.int
-- | ```
indexedArray ∷ ∀ a. String → FIndexedCodec a → ForeignCodec a
indexedArray name codec =
  C.codec'
    (\j → lmap (Named name) (C.decode codec =<< C.decode farray j))
    (\a -> C.encode farray (Array.fromFoldable (C.encode codec a)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ a. Int → ForeignCodec a → FIndexedCodec a
index ix codec = 
  C.codec
    (\xs -> lmap (AtIndex ix) (maybe (Left $ MissingValueAtIndex ix) (C.decode codec) (Array.index xs ix)))
    (pure <<< C.encode codec)

-- | Codec type for `Object` prop/value pairs.
type FPropCodec a =
  C.Codec
    (Either ForeignDecodingError)
    (FO.Object Foreign)
    (List (Tuple String Foreign))
    a
    a

-- | A codec for objects that are encoded with specific properties.
object ∷ ∀ a. String → FPropCodec a → ForeignCodec a
object name codec =
  C.codec'
    (\f → lmap (Named name) (C.decode codec =<< C.decode fobject f))
    (\a -> C.encode fobject (FO.fromFoldable (C.encode codec a)))
    -- (mapWriter (rmap (F.unsafeToForeign <<< FO.fromFoldable)))

-- | A codec for a property of an object.
prop ∷ ∀ a. String → ForeignCodec a → FPropCodec a
prop key codec = C.codec 
    (\obj → lmap (AtKey key) (maybe (Left $ MissingValueAtKey key) (C.decode codec) (FO.lookup key obj)))
    (pure <<< Tuple key <<< C.encode codec)

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example, to encode a record as the JSON object
-- | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
-- | ```
-- | import Data.Codec.Foreign as CF
-- | import Type.Proxy (Proxy(..))
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CF.ForeignCodec Person
-- | codecPerson =
-- |   CF.object "Person" $ CF.record
-- |     # CF.recordProp (Proxy :: _ "name") CF.string
-- |     # CF.recordProp (Proxy :: _ "age") CF.int
-- | ```
-- |
-- | See also `Data.Codec.Foreign.Record.object` for a more commonly useful
-- | version of this function.
record ∷ FPropCodec {}
record = C.Codec (const (pure {})) pure

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ Proxy p
  → ForeignCodec a
  → FPropCodec (Record r)
  → FPropCodec (Record r')
recordProp p codecA codecR =
  let key = reflectSymbol p in C.codec (dec' key) (enc' key)
  where
  dec'
    ∷ String
    → FO.Object Foreign 
    → (Either ForeignDecodingError) (Record r')
  dec' key obj = do
    r ← C.decode codecR obj
    a ← lmap (AtKey key) case FO.lookup key obj of
      Just val → C.decode codecA val
      Nothing → Left $ MissingValueAtKey key
    pure $ unsafeSet key a r

  enc'
    ∷ String
    → Record r'
    → List (Tuple String Foreign)
  enc' key val = 
    Tuple key (C.encode codecA (unsafeGet key val))
      : C.encode codecR (unsafeForget val)

  unsafeForget ∷ Record r' → Record r
  unsafeForget = unsafeCoerce

  unsafeSet ∷ String → a → Record r → Record r'
  unsafeSet key a = unsafeCoerce <<< FO.insert key a <<< unsafeCoerce

  unsafeGet ∷ String → Record r' → a
  unsafeGet s = unsafePartial fromJust <<< FO.lookup s <<< unsafeCoerce

-- | Used with `record` to define an optional field.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
recordPropOptional
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p (Maybe a) r r'
  ⇒ Proxy p
  → ForeignCodec a
  → FPropCodec (Record r)
  → FPropCodec (Record r')
recordPropOptional p codecA codecR = C.codec dec' enc'
  where
  key ∷ String
  key = reflectSymbol p

  dec' ∷ FO.Object Foreign → Either ForeignDecodingError (Record r')
  dec' obj = do
    r ← C.decode codecR obj
    a ← lmap (AtKey key) case FO.lookup key obj of
      Just val → Just <$> C.decode codecA val
      _ → Right Nothing
    pure $ Record.unsafeSet key a r

  enc' ∷ Record r' → List (Tuple String Foreign)
  enc' val = do
    let w = C.encode codecR (unsafeForget val)
    case Record.unsafeGet key val of
      Just a → Tuple key (C.encode codecA a) : w
      Nothing → w

  unsafeForget ∷ Record r' → Record r
  unsafeForget = unsafeCoerce


-- | Helper function for defining recursive codecs in situations where the codec
-- | definition causes a _"The value of <codec> is undefined here"_ error.
-- |
-- | ```purescript
-- | import Codec.Foreign as CF
-- | import Data.Maybe (Maybe)
-- | import Data.Newtype (class Newtype)
-- | import Data.Profunctor (wrapIso)
-- |
-- | newtype IntList = IntList { cell ∷ Int, rest ∷ Maybe IntList }
-- |
-- | derive instance newtypeLoopyList ∷ Newtype IntList _
-- |
-- | codecIntList ∷ CF.ForeignCodec IntList
-- | codecIntList =
-- |   CF.fix \codec →
-- |     wrapIso IntList $
-- |       CAR.object "IntList" { cell: CF.int, rest: CAC.maybe codec }
-- | ```
fix ∷ ∀ a. (ForeignCodec a → ForeignCodec a) → ForeignCodec a
fix f = C.codec' (\x → C.decode (f (fix f)) x) (\x → C.encode (f (fix f)) x)

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for Foreign input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `view` functions of a `Prism`-style lens.
-- |
-- | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
-- |
-- | ```purescript
-- | nonEmptyString ∷ CF.ForeignCodec NES.NonEmptyString
-- | nonEmptyString =
-- |   CF.prismaticCodec "NonEmptyString" 
-- |     NES.fromString NES.toString CF.string
-- | ```
-- |
-- | Another example might be to handle a mapping from a small sum type to
-- | strings:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: ForeignCodec Direction
-- | directionCodec = CF.prismaticCodec "Direction" dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
prismaticCodec
  ∷ ∀ a b. String → (a → Maybe b) → (b → a) → ForeignCodec a → ForeignCodec b
prismaticCodec name preview view orig = C.codec' dec enc
  where
  dec f = do
    a ← C.decode orig f
    note (Named name (UnexpectedValue (_stringify f))) (preview a)
  enc = C.encode orig <<< view

harismaticCodec
  ∷ ∀ a b
  . (a → b)
  → (b → Either Foreign a)
  → ForeignCodec a
  → ForeignCodec b
harismaticCodec view preview orig = C.codec' dec enc
  where
  dec = map view <<< C.decode orig
  enc = either identity (C.encode orig) <<< preview

encodec ∷ ∀ a. ForeignCodec a → a → Foreign
encodec codec = C.encode codec

decodec ∷ ∀ a. ForeignCodec a → Foreign → Either ForeignDecodingError a
decodec codec = C.decode codec

--
-- Helper functions 
--

decodec' ∷ ∀ a. (Foreign → Either ForeignDecodingError a) → ForeignCodec a
decodec' g = C.codec' g F.unsafeToForeign

decodingError ∷ ∀ a. F a → Either ForeignDecodingError a
decodingError = runExcept <<< withExcept ForeignDecodingErrors

--
-- Foreign imports
--

foreign import _null ∷ Foreign
foreign import _undefined ∷ Foreign
foreign import _stringify ∷ Foreign → String
