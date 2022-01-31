module Data.Witness.Pair where

import Data.Constraint (Dict(..))
import Data.Kind
import Data.Type.Equality
import Data.Witness.Constraint
import Data.Witness.Representative
import Prelude hiding ((.), id)

-- | a witness for pairs of witnesses
type PairType :: (k -> Type) -> (k -> Type) -> (k -> Type)
data PairType w1 w2 t =
    MkPairType (w1 t)
               (w2 t)

instance (Representative w1, Representative w2) => Representative (PairType w1 w2) where
    getRepWitness (MkPairType w1 w2) =
        case (getRepWitness w1, getRepWitness w2) of
            (Dict, Dict) -> Dict

instance (Is w1 t, Is w2 t) => Is (PairType w1 w2) t where
    representative = MkPairType representative representative

-- | left-biased
instance TestEquality w1 => TestEquality (PairType w1 w2) where
    testEquality (MkPairType a1 _) (MkPairType b1 _) = testEquality a1 b1

-- | right-biased
instance WitnessConstraint c w2 => WitnessConstraint c (PairType w1 w2) where
    witnessConstraint (MkPairType _ w2) = witnessConstraint w2
