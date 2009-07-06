module Data.Witness.List where
{
	import Control.Category;
	import Data.Witness.Representative;
	import Data.Witness.SimpleWitness;
	import Data.Witness.Any;
	import Data.Witness.EqualType;
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

	data ListElementType t a where
	{
		HeadListElementType :: ListElementType (h,r) h;
		TailListElementType :: ListElementType r a -> ListElementType (h,r) a;
	};

	instance SimpleWitness1 ListElementType where
	{
		matchWitness1 HeadListElementType HeadListElementType = Just MkEqualType;
		matchWitness1 (TailListElementType wa) (TailListElementType wb) = matchWitness1 wa wb;
		matchWitness1 _ _ = Nothing
	};

	getListElement :: ListElementType t a -> t -> a;
	getListElement HeadListElementType (h,_) = h;
	getListElement (TailListElementType n) (_,r) = getListElement n r;

	putListElement :: ListElementType t a -> a -> t -> t;
	putListElement HeadListElementType a (_,r) = (a,r);
	putListElement (TailListElementType n) a (h,r) = (h,putListElement n a r);

	getListElementTypes :: ListType w t -> [AnyF w (ListElementType t)];
	getListElementTypes NilListType = [];
	getListElementTypes (ConsListType w ltb) = (MkAnyF w HeadListElementType) : (fmap (\(MkAnyF wt lt) -> MkAnyF wt (TailListElementType lt)) (getListElementTypes ltb));
}
