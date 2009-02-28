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

	class SimpleWitness1 (w1 :: * -> * -> *) where
	{
		matchWitness1 :: w1 t a -> w1 t b -> Maybe (EqualType a b);
	};

	instance (SimpleWitness1 w1) => SimpleWitness (w1 t) where
	{
		matchWitness = matchWitness1;
	};
}
