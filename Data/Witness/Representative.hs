module Data.Witness.Representative where
{
	import Data.Witness.Any;

	-- every type (in Is) has a representative
	class Representative w where
	{
		withRepresentative :: forall r. (forall a. (Is w a) => w a -> r) -> (forall b. w b -> r);
	};

	class (Representative w) => Is w a where
	{
		representative :: w a;
	};

	getRepresentative :: (Is w a) => a -> w a;
	getRepresentative _ = representative;

	rerepresentative :: (Is w a) => p a -> w a;
	rerepresentative _ = representative;

	mkAny :: (Is w a) => a -> Any w;
	mkAny a = MkAny representative a;

	mkAnyF :: (Is w a) => f a -> AnyF w f;
	mkAnyF fa = MkAnyF representative fa;
}
