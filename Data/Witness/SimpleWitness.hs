module Data.Witness.SimpleWitness where
{
	import Data.Witness.EqualType;

	-- | @w@ is a simple witness type if each value witnesses to a single type. Thus if two values are the same, then they have the same type.
	;
	class SimpleWitness w where
	{
		-- | If the two values are the same, then @a@ and @b@ are the same type.
		--
		-- As an equivalence relation, 'matchWitness' must be reflexive, commutative, and transitive.
		;
		matchWitness :: w a -> w b -> Maybe (EqualType a b);
	};
}
