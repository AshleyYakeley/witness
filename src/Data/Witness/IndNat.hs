module Data.Witness.IndNat where

import Data.IndNat
import Data.Witness.Representative
import Import

type IndNatType :: IndNat -> Type
data IndNatType t where
    ZeroType :: IndNatType 'Zero
    SuccType :: IndNatType t -> IndNatType ('Succ t)

instance TestEquality IndNatType where
    testEquality ZeroType ZeroType = return Refl
    testEquality (SuccType a) (SuccType b) = do
        Refl <- testEquality a b
        return Refl
    testEquality _ _ = Nothing

instance Representative IndNatType where
    getRepWitness ZeroType = Dict
    getRepWitness (SuccType n) =
        case getRepWitness n of
            Dict -> Dict

instance Is IndNatType 'Zero where
    representative = ZeroType

instance (Is IndNatType n) => Is IndNatType ('Succ n) where
    representative = SuccType representative

type GreaterEqual :: IndNat -> IndNat -> Type
data GreaterEqual a b where
    ZeroGreaterEqual :: GreaterEqual a 'Zero
    SuccGreaterEqual :: GreaterEqual a b -> GreaterEqual ('Succ a) ('Succ b)

sameGreaterEqualWit :: IndNatType a -> GreaterEqual a a
sameGreaterEqualWit ZeroType = ZeroGreaterEqual
sameGreaterEqualWit (SuccType a) = SuccGreaterEqual $ sameGreaterEqualWit a

diff1GreaterEqualWit :: GreaterEqual a b -> GreaterEqual ('Succ a) b
diff1GreaterEqualWit ZeroGreaterEqual = ZeroGreaterEqual
diff1GreaterEqualWit (SuccGreaterEqual ge) = SuccGreaterEqual $ diff1GreaterEqualWit ge

natGreaterEqual :: IndNatType a -> IndNatType b -> Maybe (GreaterEqual a b)
natGreaterEqual _ ZeroType = return ZeroGreaterEqual
natGreaterEqual ZeroType _ = Nothing
natGreaterEqual (SuccType a) (SuccType b) = do
    al <- natGreaterEqual a b
    return $ SuccGreaterEqual al

type Add :: IndNat -> IndNat -> IndNat
type family Add a b where
    Add 'Zero b = b
    Add ('Succ a) b = 'Succ (Add a b)

addWit :: IndNatType a -> IndNatType b -> IndNatType (Add a b)
addWit ZeroType b = b
addWit (SuccType a) b = SuccType $ addWit a b

addZeroWit :: IndNatType a -> Add a 'Zero :~: a
addZeroWit ZeroType = Refl
addZeroWit (SuccType a) =
    case addZeroWit a of
        Refl -> Refl

succAddWit' :: forall a b. IndNatType a -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddWit' ZeroType = Refl
succAddWit' (SuccType a) =
    case succAddWit' @_ @b a of
        Refl -> Refl

succAddWit :: forall a b. IndNatType a -> IndNatType b -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddWit a _ = succAddWit' @a @b a

addGreaterEqualWit :: IndNatType a -> IndNatType b -> GreaterEqual (Add a b) a
addGreaterEqualWit a ZeroType =
    case addZeroWit a of
        Refl -> sameGreaterEqualWit a
addGreaterEqualWit a (SuccType b) =
    case succAddWit a b of
        Refl -> diff1GreaterEqualWit $ addGreaterEqualWit a b

type IndNatListElement :: IndNat -> forall k. [k] -> k
type family IndNatListElement n list where
    IndNatListElement 'Zero (a : aa) = a
    IndNatListElement ('Succ n) (a : aa) = IndNatListElement n aa
