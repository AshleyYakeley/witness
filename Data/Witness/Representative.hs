module Data.Witness.Representative where
{
	import Data.Witness.Any;

	-- every type (in Is) has a representative
	class Representative rep where
	{
		withRepresentative :: forall r. (forall a. (Is rep a) => rep a -> r) -> (forall b. rep b -> r);
	};

	class (Representative rep) => Is rep a where
	{
		representative :: rep a;
	};

	getRepresentative :: (Is rep a) => a -> rep a;
	getRepresentative _ = representative;

	rerepresentative :: (Is rep a) => p a -> rep a;
	rerepresentative _ = representative;

	mkAny :: (Is rep a) => a -> Any rep;
	mkAny a = MkAny representative a;

	mkAnyF :: (Is rep a) => f a -> AnyF rep f;
	mkAnyF fa = MkAnyF representative fa;
}
