module Data.Witness.EqualType where
{
	import Control.Category;
	import Prelude hiding (id,(.));

	data EqualType a a' where
	{
		MkEqualType :: EqualType a a;
	};

	instance Category EqualType where
	{
		id = MkEqualType;
		MkEqualType . MkEqualType = MkEqualType;
	};
}
