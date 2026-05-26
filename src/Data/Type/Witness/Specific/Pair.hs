module Data.Type.Witness.Specific.Pair where

import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessConstraint
import Import

-- | a witness for pairs of witnesses
type PairType :: (k -> Type) -> (k -> Type) -> (k -> Type)

type role PairType representational representational nominal

data PairType w1 w2 t
    = MkPairType
        (w1 t)
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

-- | left-biased
instance TestOrder w1 => TestOrder (PairType w1 w2) where
    testCompare (MkPairType a1 _) (MkPairType b1 _) = testCompare a1 b1

-- | right-biased
instance WitnessConstraint c w2 => WitnessConstraint c (PairType w1 w2) where
    witnessConstraint (MkPairType _ w2) = witnessConstraint w2

instance (Invariant w1, Invariant w2) => Invariant (PairType w1 w2) where
    invmap ab ba (MkPairType a1 a2) = MkPairType (invmap ab ba a1) (invmap ab ba a2)

instance (Summable w1, Summable w2) => Summable (PairType w1 w2) where
    rVoid = MkPairType rVoid rVoid
    MkPairType a1 a2 <+++> MkPairType b1 b2 = MkPairType (a1 <+++> b1) (a2 <+++> b2)

instance (Productable w1, Productable w2) => Productable (PairType w1 w2) where
    rUnit = MkPairType rUnit rUnit
    MkPairType a1 a2 <***> MkPairType b1 b2 = MkPairType (a1 <***> b1) (a2 <***> b2)
    MkPairType a1 a2 ***> MkPairType b1 b2 = MkPairType (a1 ***> b1) (a2 ***> b2)
    MkPairType a1 a2 <*** MkPairType b1 b2 = MkPairType (a1 <*** b1) (a2 <*** b2)

instance (Riggable w1, Riggable w2) => Riggable (PairType w1 w2) where
    rOptional (MkPairType a1 a2) = MkPairType (rOptional a1) (rOptional a2)
    rList1 (MkPairType a1 a2) = MkPairType (rList1 a1) (rList1 a2)
    rList (MkPairType a1 a2) = MkPairType (rList a1) (rList a2)
