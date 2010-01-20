module Data.Witness.List where
{
	import Data.Witness.Nat;
	import Data.Witness.Representative;
	import Data.Witness.SimpleWitness;
	import Data.Witness.EqualType;
	import Control.Category;
	import Prelude hiding (id,(.));

	-- | a witness type for HList-style lists. Here we use @()@ and @(,)@ for @HNil@ and @HCons@. 
	-- The @w@ parameter is the witness type of the elements.
	;
	data ListType w a where
	{
		NilListType :: ListType w ();
		ConsListType :: w a -> ListType w b -> ListType w (a,b);
	};

	instance Eq1 w => Eq1 (ListType w) where
	{
		equals1 NilListType NilListType = True;
		equals1 (ConsListType pe pl) (ConsListType qe ql) = (equals1 pe qe) && (equals1 pl ql);
		equals1 _ _ = False;
	};

	instance Eq1 w => Eq (ListType w a) where
	{
		(==) = equals1;
	};

	instance (Representative w) => Representative (ListType w) where
	{
		getRepWitness NilListType = MkRepWitness;
		getRepWitness (ConsListType w lw) = case (getRepWitness w,getRepWitness lw) of
		{
			(MkRepWitness,MkRepWitness) -> MkRepWitness;
		};
	};

	instance (Representative w) => Is (ListType w) () where
	{
		representative = NilListType;
	};

	instance (Is w a,Is (ListType w) b) => Is (ListType w) (a,b) where
	{
		representative = ConsListType representative representative;
	};

	instance (SimpleWitness w) => SimpleWitness (ListType w) where
	{
		matchWitness NilListType NilListType = Just id;
		matchWitness (ConsListType wpa wpb) (ConsListType wqa wqb) = do
		{
			MkEqualType <- matchWitness wpa wqa;
			MkEqualType <- matchWitness wpb wqb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};


    class HasListElement n list where
    {
        type ListElement n list :: *;
        getListElement :: Nat n -> list -> ListElement n list;
        putListElement :: Nat n -> ListElement n list -> list -> list;
    };

    modifyListElement :: (HasListElement n t) => Nat n -> (ListElement n t -> ListElement n t) -> t -> t;
    modifyListElement n aa t = putListElement n (aa (getListElement n t)) t;

    instance HasListElement Zero (a,r) where
    {
        type ListElement Zero (a,r) = a;
        getListElement _ (a,_) = a;
        putListElement _ a (_,r) = (a,r);
    };

    instance (HasListElement n r) => HasListElement (Succ n) (a,r) where
    {
        type ListElement (Succ n) (a,r) = ListElement n r;
        getListElement (SuccNat n) (_,r) = getListElement n r;
        getListElement _ _ = undefined;    -- hack to overcome dumb warning
        putListElement (SuccNat n) a (f,r) = (f,putListElement n a r);
        putListElement _ _ _ = undefined;    -- hack to overcome dumb warning
    };
}
