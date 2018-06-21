module Data.Witness.Nat where

import Data.Constraint (Dict(..))
import Data.Maybe
import Data.Nat
import Data.Type.Equality
import Data.Witness.Representative
import Prelude hiding ((.), id)

data NatType (t :: Nat) where
    ZeroType :: NatType 'Zero
    SuccType :: NatType t -> NatType ('Succ t)

instance TestEquality NatType where
    testEquality ZeroType ZeroType = return Refl
    testEquality (SuccType a) (SuccType b) = do
        Refl <- testEquality a b
        return Refl
    testEquality _ _ = Nothing

instance Representative NatType where
    getRepWitness ZeroType = Dict
    getRepWitness (SuccType n) =
        case getRepWitness n of
            Dict -> Dict

instance Is NatType 'Zero where
    representative = ZeroType

instance (Is NatType n) => Is NatType ('Succ n) where
    representative = SuccType representative

data GreaterEqual (a :: Nat) (b :: Nat) where
    ZeroGreaterEqual :: GreaterEqual a 'Zero
    SuccGreaterEqual :: GreaterEqual a b -> GreaterEqual ('Succ a) ('Succ b)

sameGreaterEqualWit :: NatType a -> GreaterEqual a a
sameGreaterEqualWit ZeroType = ZeroGreaterEqual
sameGreaterEqualWit (SuccType a) = SuccGreaterEqual $ sameGreaterEqualWit a

diff1GreaterEqualWit :: GreaterEqual a b -> GreaterEqual ('Succ a) b
diff1GreaterEqualWit ZeroGreaterEqual = ZeroGreaterEqual
diff1GreaterEqualWit (SuccGreaterEqual ge) = SuccGreaterEqual $ diff1GreaterEqualWit ge

natGreaterEqual :: NatType a -> NatType b -> Maybe (GreaterEqual a b)
natGreaterEqual _ ZeroType = return ZeroGreaterEqual
natGreaterEqual ZeroType _ = Nothing
natGreaterEqual (SuccType a) (SuccType b) = do
    al <- natGreaterEqual a b
    return $ SuccGreaterEqual al

type family Add (a :: Nat) (b :: Nat) :: Nat where
    Add 'Zero b = b
    Add ('Succ a) b = 'Succ (Add a b)

addWit :: NatType a -> NatType b -> NatType (Add a b)
addWit ZeroType b = b
addWit (SuccType a) b = SuccType $ addWit a b

addZeroWit :: NatType a -> Add a 'Zero :~: a
addZeroWit ZeroType = Refl
addZeroWit (SuccType a) =
    case addZeroWit a of
        Refl -> Refl

succAddWit' :: forall a b. NatType a -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddWit' ZeroType = Refl
succAddWit' (SuccType a) =
    case succAddWit' @_ @b a of
        Refl -> Refl

succAddWit :: forall a b. NatType a -> NatType b -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddWit a _ = succAddWit' @a @b a

addGreaterEqualWit :: NatType a -> NatType b -> GreaterEqual (Add a b) a
addGreaterEqualWit a ZeroType =
    case addZeroWit a of
        Refl -> sameGreaterEqualWit a
addGreaterEqualWit a (SuccType b) =
    case succAddWit a b of
        Refl -> diff1GreaterEqualWit $ addGreaterEqualWit a b
