module Data.Witness.SameType where
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
}
