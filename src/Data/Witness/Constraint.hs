module Data.Witness.Constraint where

import Data.Constraint
import Data.Functor.Compose
import Data.Kind
import Data.Type.Equality
import Prelude

type AllWitnessConstraint :: forall kw kt. (kw -> Constraint) -> (kt -> kw) -> Constraint
class AllWitnessConstraint c w where
    allWitnessConstraint :: forall t. Dict (c (w t))

instance AllWitnessConstraint Show ((:~:) t) where
    allWitnessConstraint = Dict

showAllWitness ::
       forall k (w :: k -> Type) (t :: k). AllWitnessConstraint Show w
    => w t
    -> String
showAllWitness wt =
    case allWitnessConstraint @_ @_ @Show @w @t of
        Dict -> show wt

type WitnessConstraint :: forall k. (k -> Constraint) -> (k -> Type) -> Constraint
class WitnessConstraint c w where
    witnessConstraint :: forall t. w t -> Dict (c t)

instance WitnessConstraint c (Compose Dict c) where
    witnessConstraint (Compose d) = d
