module Data.Witness.EqualType where
{
	import Control.Category;
	import Prelude hiding (id,(.));

	-- | witness that type parameters @a@ and @b@ are the same type
	;
	data EqualType a b where
	{
		MkEqualType :: EqualType t t;
	};

	instance Category EqualType where
	{
		id = MkEqualType;
		MkEqualType . MkEqualType = MkEqualType;
	};
}
