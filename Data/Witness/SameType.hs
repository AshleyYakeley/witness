module Data.Witness.SameType where
{
	import Control.Category;
	import Prelude hiding (id,(.));

	newtype SameType a a' = MkAllMap (forall f. f a -> f a');

	instance Category SameType where
	{
		id = MkAllMap id;
		(MkAllMap bc) . (MkAllMap ab) = MkAllMap (bc . ab);
	};

	newtype ID a = ID {unID :: a};
	mapID :: SameType a a' -> a -> a';
	mapID (MkAllMap mp) = unID . mp . ID;

	newtype Compose p q a = MkCompose {unCompose :: p (q a)};
	mapCompose :: SameType a a' -> SameType (q a) (q a');
	mapCompose (MkAllMap mp) = MkAllMap (unCompose . mp . MkCompose);

	newtype Compose2 p q b a = MkCompose2 {unCompose2 :: p (q a b)};
	mapCompose2 :: SameType a a' -> SameType (q a b) (q a' b);
	mapCompose2 (MkAllMap mp) = MkAllMap (unCompose2 . mp . MkCompose2);
}
