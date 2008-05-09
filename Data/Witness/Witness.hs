module Data.Witness.Witness where
{
	import Data.Witness.SameType;

	-- | If two witnesses have the same value, then they have the same type.
	;
	class Witness w where
	{
		matchWitness :: w a -> w a' -> Maybe (SameType a a');
	};
}
