module Data.Witness.SameType
(
	SameType(..),--TT(..),
	mapSameType,reverseSameType,lift1SameType,lift2SameType,change1SameType,apply1SameType
) where
{
	import Control.Category;
	import Prelude hiding (id,(.));

	newtype SameType a a' = MkSameType {unSameType :: forall p. p a -> p a'};

	instance Category SameType where
	{
		id = MkSameType id;
		(MkSameType bc) . (MkSameType ab) = MkSameType (bc . ab);
	};

	newtype Id a = MkId {unId :: a};
	mapSameType :: SameType a a' -> a -> a';
	mapSameType (MkSameType mp) = unId . mp . MkId;
	
	newtype Reverse f b a = MkReverse {unReverse :: f a -> b};
	reverseSameType :: SameType a a' -> SameType a' a;
	reverseSameType (MkSameType mp) = MkSameType (unReverse (mp (MkReverse id)));
	
	newtype Compose p q a = MkCompose {unCompose :: p (q a)};
	lift1SameType :: SameType a a' -> SameType (p a) (p a');
	lift1SameType (MkSameType fafa) = MkSameType (unCompose . fafa . MkCompose);

	newtype Compose2 p q b a = MkCompose2 {unCompose2 :: p (q a b)};
	lift2SameType :: SameType a a' -> SameType (q a b) (q a' b);
	lift2SameType (MkSameType mp) = MkSameType (unCompose2 . mp . MkCompose2);
	
--	newtype TT a where
--	{
--		MkTT :: TT Int;
--	};
	
	data Change1 f b pa where
	{ 
		MkChange1 :: f (p b) -> Change1 f b (p a)
	};
	unChange1 :: Change1 f b (p a) -> f (p b);
	unChange1 (MkChange1 n) = n;

	change1SameType :: SameType (p a) (p' a) -> SameType (p b) (p' b);
	change1SameType (MkSameType fpafpa) = MkSameType (unChange1 . fpafpa . MkChange1);
	
	apply1SameType :: SameType (p ()) (p' ()) -> SameType a a' -> SameType (p a) (p' a');
	apply1SameType spp saa = (change1SameType spp) . (lift1SameType saa);
}
