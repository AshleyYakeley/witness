module Data.Witness.General.ListElement where

import Data.Witness.General.Representative
import Data.Witness.Specific.List.Element
import Data.Witness.Specific.List.List
import Import

type ListElementWitness :: forall k. (k -> Type) -> Constraint
class Is (ListType Proxy) (WitnessTypeList w) => ListElementWitness (w :: k -> Type) where
    type WitnessTypeList w :: [k]
    toFiniteConsElement :: forall t. w t -> ListElementType (WitnessTypeList w) t
    fromFiniteConsElement :: forall t. ListElementType (WitnessTypeList w) t -> w t

instance Is (ListType Proxy) tt => ListElementWitness (ListElementType tt) where
    type WitnessTypeList (ListElementType tt) = tt
    toFiniteConsElement = id
    fromFiniteConsElement = id
