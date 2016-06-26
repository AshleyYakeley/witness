module Control.Category.Tensor where
{
    import Data.Semigroupoid.Dual;

    -- could use data-lens:Control.Category.Product(Tensor)
    class Tensor cc where
    {
        tensorUnit :: cc () ();
        tensorPair :: cc a1 b1 -> cc a2 b2 -> cc (a1,a2) (b1,b2);
    };

    instance Tensor (->) where
    {
        tensorUnit = id;
        tensorPair ab1 ab2 (a1,a2) = (ab1 a1,ab2 a2);
    };

    instance (Tensor cc) => Tensor (Dual cc) where
    {
        tensorUnit = Dual tensorUnit;
        tensorPair (Dual ab1) (Dual ab2) = Dual (tensorPair ab1 ab2);
    };
}