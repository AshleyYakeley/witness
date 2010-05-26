module Data.Nothing where
{
    data Nothing;

    never :: Nothing -> a;
    never n = seq n undefined;

    instance Eq Nothing where
    {
        a == _b = never a;
    };

    instance Ord Nothing where
    {
        a <= _b = never a;
    };

    instance Show Nothing where
    {
        show a = never a;
    };
}
