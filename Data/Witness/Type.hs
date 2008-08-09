module Data.Witness.Type where
{
	import Data.Witness.Representative;

	-- | The simplest 'Representative', with one value that represents all types.
	;
	data Type a = Type;

	instance Eq1 Type where
	{
		equals1 Type Type = True;
	};

	instance Eq (Type a) where
	{
		(==) = equals1;
	};

	instance Representative Type where
	{
		withRepresentative f t = f t;
	};

	instance Is Type a where
	{
		representative = Type;
	};
}
