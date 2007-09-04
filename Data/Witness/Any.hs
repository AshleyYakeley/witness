module Data.Witness.Any where
{
	data Any w where
	{
		Any :: forall a. w a -> a -> Any w
	};

	data AnyF w f where
	{
		AnyF :: forall a. w a -> f a -> AnyF w f
	};

	data AnyWitness w where
	{
		AnyWitness :: forall a. w a -> AnyWitness w
	};
}
