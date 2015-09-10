module Data.Witness.ListElement where
{
    import Data.Witness.Nat;

    class HasListElement (n :: NatKind) (list :: *) where
    {
        type ListElement n list :: *;
        getListElement :: Nat n -> list -> ListElement n list;
        putListElement :: Nat n -> ListElement n list -> list -> list;
    };

    modifyListElement :: (HasListElement n t) => Nat n -> (ListElement n t -> ListElement n t) -> t -> t;
    modifyListElement n aa t = putListElement n (aa (getListElement n t)) t;

    instance HasListElement 'Zero (a,r) where
    {
        type ListElement 'Zero (a,r) = a;
        getListElement _ (a,_) = a;
        putListElement _ a (_,r) = (a,r);
    };

    instance (HasListElement n r) => HasListElement ('Succ n) (a,r) where
    {
        type ListElement ('Succ n) (a,r) = ListElement n r;
        getListElement (SuccNat n) (_,r) = getListElement n r;
        putListElement (SuccNat n) a (f,r) = (f,putListElement n a r);
    };
}
