module Test.Examples.Newtype where

import Prelude

import Data.Codec.Foreign.Common as F
import Data.Codec.Foreign.Record as FR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (Foreign)

foreign import sample :: Foreign

type PersonRec = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

newtype Person = Person PersonRec
instance Show Person where
  show (Person p) =
    "(Person { name:" <> show p."Name"
      <> ", age:"
      <> show p.age
      <> ", is_active:"
      <> show p."is active"
      <> "})"

derive instance newtypePerson ∷ Newtype Person _

codec ∷ F.ForeignCodec Person
codec =
  wrapIso Person
    (FR.object "Person"
      { "Name": F.string
      , age: F.int
      , "is active": F.boolean
      })

-- > spago run -m Test.Examples.Newtype
-- (Right (Person { name:"John", age:42, is_active:true}))

main :: Effect Unit
main = do
  -- let person = Person { "Name": "John", age: 42, "is active": true }
  logShow $ F.decode codec $ sample

