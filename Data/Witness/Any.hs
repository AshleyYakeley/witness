module Data.Witness.Any where
{
	data Any w where
	{
		MkAny :: forall a. w a -> a -> Any w
	};

	data AnyF w f where
	{
		MkAnyF :: forall a. w a -> f a -> AnyF w f
	};

	data AnyWitness w where
	{
		MkAnyWitness :: forall a. w a -> AnyWitness w
	};
}
