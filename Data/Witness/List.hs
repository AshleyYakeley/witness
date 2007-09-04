module Data.Witness.List where
{
	import Control.Compositor;
	import Data.Witness.Representative;
	import Data.Witness.Witness;
	import Data.Witness.AllMap;

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
		matchWitnessF NilListType NilListType = Just identity;
		matchWitnessF (ConsListType wpa wpb) (ConsListType wqa wqb) = do
		{
			fa <- matchWitnessF wpa wqa;
			fb <- matchWitnessF wpb wqb;
			return (compose (mapCompose2 fa) (mapCompose fb));
		};
		matchWitnessF _ _ = Nothing;
	};
}
