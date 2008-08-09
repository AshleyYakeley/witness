module Data.Witness.Witness where
{
	import Data.Witness.EqualType;

	-- | @w@ is a witness type if if has the property that if two values are the same, then they have the same type.
	;
	class Witness w where
	{
		-- | If the two values are the same, then @a@ and @b@ are the same type.
		--
		-- As an equivalence relation, 'matchWitness' must be reflexive, commutative, and transitive.
		;
		matchWitness :: w a -> w b -> Maybe (EqualType a b);
	};
}
