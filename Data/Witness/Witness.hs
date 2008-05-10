module Data.Witness.Witness where
{
	import Data.Witness.EqualType;

	-- | If two witnesses have the same value, then they have the same type.
	;
	class Witness w where
	{
		matchWitness :: w a -> w a' -> Maybe (EqualType a a');
	};
}
