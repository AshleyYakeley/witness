module Data.Witness.Nat where
{
    import Prelude hiding (id,(.));
    import Data.Maybe;
    import Data.Type.Equality;
    import Data.Constraint(Dict(..));
    import Data.Witness.Representative;

    data NatKind = Zero | Succ NatKind;

    data Nat (t :: NatKind) where
    {
        ZeroNat :: Nat Zero;
        SuccNat :: Nat t -> Nat (Succ t);
    };

    instance TestEquality Nat where
    {
        testEquality ZeroNat ZeroNat = return Refl;
        testEquality (SuccNat a) (SuccNat b) = do
        {
            Refl <- testEquality a b;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };

    instance Eq1 Nat where
    {
        equals1 a b = isJust (testEquality a b);
    };

    instance Representative Nat where
    {
        getRepWitness ZeroNat = Dict;
        getRepWitness (SuccNat n) = case getRepWitness n of
        {
            Dict -> Dict;
        };
    };

    instance Is Nat Zero where
    {
        representative = ZeroNat;
    };

    instance (Is Nat n) => Is Nat (Succ n) where
    {
        representative = SuccNat representative;
    };
}
