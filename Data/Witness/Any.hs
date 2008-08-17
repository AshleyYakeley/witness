module Data.Witness.Any where
{
	import Data.Witness.SimpleWitness;
	import Data.Witness.EqualType;
	import Data.Maybe;

	-- | Any value with a witness to it.
	;
	data Any w = forall a. MkAny (w a) a;
	
	matchAny :: (SimpleWitness w) => w a -> Any w -> Maybe a;
	matchAny wit (MkAny cwit ca) = do
	{
		MkEqualType <- matchWitness cwit wit;
		return ca;
	};

	-- | Any value with a witness to a parameter of its type.
	;
	data AnyF w f = forall a. MkAnyF (w a) (f a);
	
	matchAnyF :: (SimpleWitness w) => w a -> AnyF w f -> Maybe (f a);
	matchAnyF wit (MkAnyF cwit cfa) = do
	{
		MkEqualType <- matchWitness cwit wit;
		return cfa;
	};

	data AnyF1 w f = forall (a :: * -> *). MkAnyF1 (w a) (f a);
	data AnyF2 w f = forall (a :: * -> * -> *). MkAnyF2 (w a) (f a);

	-- | Any witness.
	;
	data AnyWitness w = forall a. MkAnyWitness (w a);
	
	matchAnyWitness :: (SimpleWitness w) => w a -> AnyWitness w -> Bool;
	matchAnyWitness wit (MkAnyWitness cwit) = isJust (matchWitness cwit wit);

	instance (SimpleWitness w) => Eq (AnyWitness w) where
	{
		(==) (MkAnyWitness wa) = matchAnyWitness wa;
	};

	data AnyWitness1 w = forall (a :: * -> *). MkAnyWitness1 (w a);
	data AnyWitness2 w = forall (a :: * -> * -> *). MkAnyWitness2 (w a);
}
