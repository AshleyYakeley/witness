module Data.Witness.List where
{
	import Control.Category;
	import Data.Witness.Representative;
	import Data.Witness.Witness;
	import Data.Witness.SameType;
	import Prelude hiding (id,(.));

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
		withRepresentative f NilListType = f NilListType;
		withRepresentative f (ConsListType w lw) = withRepresentative (\w' -> withRepresentative (\lw' -> f (ConsListType w' lw')) lw) w;
	};

	instance (Representative w) => Is (ListType w) () where
	{
		representative = NilListType;
	};

	instance (Is w a,Is (ListType w) b) => Is (ListType w) (a,b) where
	{
		representative = ConsListType representative representative;
	};

	instance (Witness w) => Witness (ListType w) where
	{
		matchWitness NilListType NilListType = Just id;
		matchWitness (ConsListType wpa wpb) (ConsListType wqa wqb) = do
		{
			MkSameType <- matchWitness wpa wqa;
			MkSameType <- matchWitness wpb wqb;
			return MkSameType;
		};
		matchWitness _ _ = Nothing;
	};
}
