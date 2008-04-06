module Data.Witness.Any where
{
	data Any w = forall a. MkAny (w a) a;

	data AnyF w f = forall a. MkAnyF (w a) (f a);

	data AnyWitness w = forall a. MkAnyWitness (w a);
}
