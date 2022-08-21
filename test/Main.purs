module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Codec ((~))
import Data.Codec as C
import Data.Codec.Foreign as F
import Data.Codec.Foreign.Common as FC
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as Object
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Primitive Codecs" do
    describe "encode" do
      it "boolean true" do
        C.encode F.boolean true `shouldEqualF` true
      it "boolean false" do
        C.encode F.boolean false `shouldEqualF` false
      it "number" do
        C.encode F.number someNumber `shouldEqualF` someNumber
      it "array" do
        C.encode (F.array F.boolean) someBools `shouldEqualF` someBools
      it "indexed array" do
        C.encode codecPerson somePerson `shouldEqualF` personToArray somePerson
    describe "decode" do
      it "boolean true" do
        C.decode F.boolean (unsafeToForeign true) `shouldEqual` pure true
      it "boolean false" do
        C.decode F.boolean (unsafeToForeign false) `shouldEqual` pure false
      it "number" do
        C.decode F.number (unsafeToForeign someNumber)
          `shouldEqual` pure someNumber
      it "array" do
        C.decode (F.array F.boolean) (unsafeToForeign someBools)
          `shouldEqual` pure someBools
      it "indexed array" do
        C.decode codecPerson (personToArray somePerson)
          `shouldEqual` pure somePerson

--
-- Test Fixture
-- 

newtype Person = Person { name ∷ String, age ∷ Int, parent ∷ Maybe Person }

derive instance Eq Person
derive instance Newtype Person _
derive instance Generic Person _
instance Show Person where
  show (Person p) =
    "(Person { name:" <> show p.name
      <> ", age:"
      <> show p.age
      <> ", parent:"
      <> show p.parent
      <> "})"

codecPerson ∷ F.ForeignCodec Person
codecPerson = F.fix \(codec ∷ F.ForeignCodec Person) →
  F.indexedArray "Person" (person codec)
  where
  person ∷ F.ForeignCodec Person → F.FIndexedCodec Person
  person codec = wrapIso Person ado
    name ← _.name ~ F.index 0 F.string
    age ← _.age ~ F.index 1 F.int
    parent ← _.parent ~ F.index 2 (FC.maybe codec)
    in { name, age, parent }

somePerson ∷ Person
somePerson = Person
  { name: "Abel"
  , age: 40
  , parent: Just $ Person
      { name: "Adam"
      , age: 11
      , parent: Nothing
      }
  }

personToArray ∷ Person → Foreign
personToArray (Person { name, age, parent }) =
  unsafeToForeign
    [ unsafeToForeign name
    , unsafeToForeign age
    , unsafeToForeign case parent of
        Nothing → Object.singleton "tag" (unsafeToForeign "Nothing")
        Just p → Object.singleton "tag" (unsafeToForeign "Just")
          # Object.insert "value" (personToArray p)
    ]

someNumber ∷ Number
someNumber = 12345678987654321.42

someBools ∷ Array Boolean
someBools = [ true, false, true, false, false, true ]

foreign import stringify ∷ Foreign → String

shouldEqualF ∷ ∀ m a. Applicative m ⇒ MonadThrow Error m ⇒ Foreign → a → m Unit
shouldEqualF l r = stringify l `shouldEqual` stringify (unsafeToForeign r)
