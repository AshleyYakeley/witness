module Data.Witness.Type where
{
	import Data.Witness.Representative;

	data Type a = Type;

	instance Eq (Type a) where
	{
		Type == Type = True;
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
