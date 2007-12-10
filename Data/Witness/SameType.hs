module Data.Witness.SameType
(
	SameType(..),
	mapSameType,reverseSameType,lift1SameType,lift2SameType,change1SameType,apply1SameType
)
 where
{
	import Control.Category;
	import Prelude hiding (id,(.));

	data SameType a a' where
	{
		MkSameType :: SameType a a;
	};

	instance Category SameType where
	{
		id = MkSameType;
		MkSameType . MkSameType = MkSameType;
	};

	mapSameType :: SameType a a' -> a -> a';
	mapSameType MkSameType a = a;
	
	reverseSameType :: SameType a a' -> SameType a' a;
	reverseSameType MkSameType = MkSameType;
	
	lift1SameType :: SameType a a' -> SameType (p a) (p a');
	lift1SameType MkSameType = MkSameType;

	lift2SameType :: SameType a a' -> SameType (q a b) (q a' b);
	lift2SameType MkSameType = MkSameType;

	change1SameType :: SameType (p a) (p' a) -> SameType (p b) (p' b);
	change1SameType MkSameType = MkSameType;
	
	apply1SameType :: SameType (p ()) (p' ()) -> SameType a a' -> SameType (p a) (p' a');
	apply1SameType MkSameType MkSameType = MkSameType;
}
