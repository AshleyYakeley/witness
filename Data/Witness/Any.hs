module Data.Witness.Any where
{
	import Data.Witness.Witness;
	import Data.Witness.EqualType;
	import Data.Maybe;

	data Any w = forall a. MkAny (w a) a;
	
	matchAny :: (Witness w) => w a -> Any w -> Maybe a;
	matchAny wit (MkAny cwit ca) = do
	{
		MkEqualType <- matchWitness cwit wit;
		return ca;
	};

	data AnyF w f = forall a. MkAnyF (w a) (f a);
	
	matchAnyF :: (Witness w) => w a -> AnyF w f -> Maybe (f a);
	matchAnyF wit (MkAnyF cwit cfa) = do
	{
		MkEqualType <- matchWitness cwit wit;
		return cfa;
	};

	data AnyWitness w = forall a. MkAnyWitness (w a);
	
	matchAnyWitness :: (Witness w) => w a -> AnyWitness w -> Bool;
	matchAnyWitness wit (MkAnyWitness cwit) = isJust (matchWitness cwit wit);

	instance (Witness w) => Eq (AnyWitness w) where
	{
		(==) (MkAnyWitness wa) = matchAnyWitness wa;
	};
}
