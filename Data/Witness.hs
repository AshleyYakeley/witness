module Data.Witness where
{
	class Compositor comp where
	{
		identity :: comp a a;
		compose :: comp b c -> comp a b -> comp a c;
	};

	instance Compositor (->) where
	{
		identity = id;
		compose = (.);
	};

	newtype AllMap a a' = MkAllMap (forall f. f a -> f a');

	instance Compositor AllMap where
	{
		identity = MkAllMap identity;
		compose (MkAllMap bc) (MkAllMap ab) = MkAllMap (compose bc ab);
	};

	newtype ID a = ID {unID :: a};
	mapID :: AllMap a a' -> a -> a';
	mapID (MkAllMap mp) = unID . mp . ID;

	newtype Compose p q a = MkCompose {unCompose :: p (q a)};
	mapCompose :: AllMap a a' -> AllMap (q a) (q a');
	mapCompose (MkAllMap mp) = MkAllMap (unCompose . mp . MkCompose);

	newtype Compose2 p q b a = MkCompose2 {unCompose2 :: p (q a b)};
	mapCompose2 :: AllMap a a' -> AllMap (q a b) (q a' b);
	mapCompose2 (MkAllMap mp) = MkAllMap (unCompose2 . mp . MkCompose2);

	class Witness w where
	{
		withWitness :: forall r. (forall a. (Is w a) => w a -> r) -> (forall b. w b -> r);
	};

	class (Witness w) => Is w a where
	{
		witness :: w a;
	};

	getWitness :: (Is w a) => a -> w a;
	getWitness _ = witness;

	rewitness :: (Is w a) => p a -> w a;
	rewitness _ = witness;

	class (Witness w) => TotalWitness w where
	{
		matchWitnessF :: w a -> w a' -> Maybe (AllMap a a');
	};

	matchIsF :: (TotalWitness w,Is w a,Is w a') => Type (w ()) -> Maybe (AllMap a a');
	matchIsF t = matchWitnessF (foo t) (foo t) where
	{
		foo :: (Is w a) => Type (w ()) -> w a;
		foo _ = witness;
	};

	matchWitness :: (TotalWitness w) => w a -> w a' -> Maybe (a -> a');
	matchWitness wa wb = matchWitnessF wa wb >>= (\mapF -> return (mapID mapF));

	matchIs :: (TotalWitness w,Is w a,Is w a') => Type (w ()) -> Maybe (a -> a');
	matchIs t = matchIsF t >>= (\mapF -> return (mapID mapF));

	data Type a = Type;

	instance Eq (Type a) where
	{
		Type == Type = True;
	};

	instance Witness Type where
	{
		withWitness f t = f t;
	};

	instance Is Type a where
	{
		witness = Type;
	};

	data Any w where
	{
		Any :: forall a. (Is w a) => a -> Any w
	};

	data AnyF w f where
	{
		AnyF :: forall a. (Is w a) => f a -> AnyF w f
	};

	data AnyWitness w where
	{
		AnyWitness :: forall a. w a -> AnyWitness w
	};


	-- ListType

	data ListType w a where
	{
		NilListType :: ListType w ();
		ConsListType :: w a -> ListType w b -> ListType w (a,b);
	};

	instance (Witness w) => Witness (ListType w) where
	{
		withWitness f NilListType = f NilListType;
		withWitness f (ConsListType w lw) = withWitness (\w' -> withWitness (\lw' -> f (ConsListType w' lw')) lw) w;
	};

	instance (Witness w) => Is (ListType w) () where
	{
		witness = NilListType;
	};

	instance (Witness w,Is w a,Is (ListType w) b) => Is (ListType w) (a,b) where
	{
		witness = ConsListType witness witness;
	};

	instance (TotalWitness w) => TotalWitness (ListType w) where
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
