module Data.Witness.Nat where
{
    import Data.Witness.Representative;
    import Data.Witness.SimpleWitness;
    import Data.Witness.EqualType;
    import Data.Constraint(Dict(..));
    import Data.Maybe;
    import Prelude hiding (id,(.));

    data NatKind = Zero | Succ NatKind;

    data Nat (t :: NatKind) where
    {
        ZeroNat :: Nat Zero;
        SuccNat :: Nat t -> Nat (Succ t);
    };

    instance SimpleWitness Nat where
    {
        matchWitness ZeroNat ZeroNat = return MkEqualType;
        matchWitness (SuccNat a) (SuccNat b) = do
        {
            MkEqualType <- matchWitness a b;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

    instance Eq1 Nat where
    {
        equals1 a b = isJust (matchWitness a b);
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
