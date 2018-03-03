module Data.Witness.Constraint where

import Data.Constraint
import Data.Type.Equality

class AllWitnessConstraint (c :: kw -> Constraint) (w :: kt -> kw) where
    allWitnessConstraint :: forall (t :: kt). Dict (c (w t))

instance AllWitnessConstraint Show ((:~:) t) where
    allWitnessConstraint = Dict

showAllWitness ::
       forall w t. AllWitnessConstraint Show w
    => w t
    -> String
showAllWitness wt =
    case allWitnessConstraint @_ @_ @Show @w @t of
        Dict -> show wt

class WitnessConstraint (c :: k -> Constraint) (w :: k -> *) where
    witnessConstraint :: forall (t :: k). w t -> Dict (c t)
