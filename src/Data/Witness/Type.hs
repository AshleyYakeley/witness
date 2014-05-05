module Data.Witness.Type where
{
    import Data.Witness.Representative;
    import Data.Constraint;

    -- | The simplest 'Representative', with one value that represents all types.
    ;
    data Type (a :: k) = Type;

    instance Eq1 Type where
    {
        equals1 Type Type = True;
    };

    instance Eq (Type a) where
    {
        (==) = equals1;
    };

    instance Representative Type where
    {
        getRepWitness Type = Dict;
    };

    instance Is Type a where
    {
        representative = Type;
    };
}
