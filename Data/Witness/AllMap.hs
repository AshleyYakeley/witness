module Data.Witness.AllMap where
{
	import Control.Compositor;

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
}
