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
			fa <- matchWitness wpa wqa;
			fb <- matchWitness wpb wqb;
			return ((lift2SameType fa) . (lift1SameType fb));
		};
		matchWitness _ _ = Nothing;
	};
}
