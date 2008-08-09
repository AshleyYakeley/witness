module Data.Witness.Representative where
{
	import Data.Witness.Any;

	class Eq1 p where
	{
		equals1 :: forall a. p a -> p a -> Bool;
	};

	class Eq1 rep => Representative rep where
	{
		-- | Every value is an instance of 'Is'.
		;
		withRepresentative :: forall r. (forall a. (Is rep a) => rep a -> r) -> (forall b. rep b -> r);
	};

	-- | If two representatives have the same type, then they have the same value.
	;
	class Representative rep => Is rep a where
	{
		-- | The representative value for type @a@.
		;
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
