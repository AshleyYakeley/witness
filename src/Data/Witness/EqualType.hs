module Data.Witness.EqualType where
{
    import Control.Category;

    -- | witness that type parameters @a@ and @b@ are the same type
    ;
    data EqualType :: k -> k -> * where
    {
        MkEqualType :: EqualType t t;
    };

    instance Category EqualType where
    {
        id = MkEqualType;
        MkEqualType . MkEqualType = MkEqualType;
    };
}
