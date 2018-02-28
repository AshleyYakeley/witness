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

instance Eq1 NatType where
    equals1 a b = isJust (testEquality a b)

instance Representative NatType where
    getRepWitness ZeroType = Dict
    getRepWitness (SuccType n) =
        case getRepWitness n of
            Dict -> Dict

instance Is NatType 'Zero where
    representative = ZeroType

instance (Is NatType n) => Is NatType ('Succ n) where
    representative = SuccType representative
