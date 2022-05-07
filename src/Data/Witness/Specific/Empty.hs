module Data.Witness.Specific.Empty where

import Data.Witness.General.Finite
import Data.Witness.General.ListElement
import Data.Witness.General.Representative
import Data.Witness.General.WitnessConstraint
import Data.Witness.Specific.All
import Import

type EmptyType :: forall k. k -> Type
newtype EmptyType t =
    MkEmptyType Void
    deriving (Eq, Countable, Searchable, Empty)

instance Finite (EmptyType t) where
    allValues = []
    assemble _ = pure never

instance TestEquality EmptyType where
    testEquality = never

instance Representative EmptyType where
    getRepWitness = never

instance FiniteWitness EmptyType where
    assembleWitnessFor _ = pure emptyAllFor

instance WitnessConstraint c EmptyType where
    witnessConstraint = never

instance ListElementWitness EmptyType where
    type WitnessTypeList EmptyType = '[]
    toFiniteConsElement wit = never wit
    fromFiniteConsElement lt = never lt

emptyAllOf :: AllOf EmptyType
emptyAllOf = MkAllOf never

emptyAllFor :: AllFor f EmptyType
emptyAllFor = MkAllFor never
