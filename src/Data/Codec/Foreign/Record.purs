module Data.Codec.Foreign.Record where

import Data.Codec.Foreign as F
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Constructs a `JsonCodec` for a `Record` from a name and a record of codecs.
-- | The name is used in the error message produced when decoding fails.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CA.JsonCodec Person
-- | personCodec = CAR.object "Person" { name: CA.string, age: CA.int }
-- | ```
object
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ String
  → Record ri
  → F.ForeignCodec (Record ro)
object name rec = F.object name (record rec)

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `JsonCodec` instead.
record
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → F.FPropCodec (Record ro)
record = rowListCodec (Proxy ∷ Proxy rl)

-- | Used to wrap codec values provided in `record` to indicate the field is optional.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
newtype Optional a = Optional (F.ForeignCodec a)

-- | A lowercase alias for `Optional`, provided for stylistic reasons only.
optional ∷ ∀ a. F.ForeignCodec a → Optional a
optional = Optional

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  rowListCodec ∷ ∀ proxy. proxy rl → Record ri → F.FPropCodec (Record ro)

instance rowListCodecNil ∷ RowListCodec RL.Nil () () where
  rowListCodec _ _ = F.record

instance rowListCodecConsOptional ∷
  ( RowListCodec rs ri' ro'
  , R.Cons sym (Optional a) ri' ri
  , R.Cons sym (Maybe a) ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (Optional a) rs) ri ro where
  rowListCodec _ codecs =
    F.recordPropOptional (Proxy ∷ Proxy sym) codec tail
    where
    codec ∷ F.ForeignCodec a
    codec = coerce (Rec.get (Proxy ∷ Proxy sym) codecs ∷ Optional a)

    tail ∷ F.FPropCodec (Record ro')
    tail = rowListCodec (Proxy ∷ Proxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)

else instance rowListCodecCons ∷
  ( RowListCodec rs ri' ro'
  , R.Cons sym (F.ForeignCodec a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (F.ForeignCodec a) rs) ri ro where
  rowListCodec _ codecs =
    F.recordProp (Proxy ∷ Proxy sym) codec tail
    where
    codec ∷ F.ForeignCodec a
    codec = Rec.get (Proxy ∷ Proxy sym) codecs

    tail ∷ F.FPropCodec (Record ro')
    tail = rowListCodec (Proxy ∷ Proxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
