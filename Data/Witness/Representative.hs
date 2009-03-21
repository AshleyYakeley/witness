module Data.Witness.Representative where
{
	import Data.Witness.Any;
	import Data.Witness.SimpleWitness;

	class Eq1 p where
	{
		equals1 :: forall a. p a -> p a -> Bool;
	};

	data RepWitness rep a where
	{
		MkRepWitness :: (Is rep a) => RepWitness rep a;
	};

	isWitnessRepresentative :: RepWitness rep a -> rep a;
	isWitnessRepresentative MkRepWitness = representative;

	instance (SimpleWitness w) => SimpleWitness (RepWitness w) where
	{
		matchWitness wa wb = matchWitness (isWitnessRepresentative wa) (isWitnessRepresentative wb);
	};

	class Eq1 rep => Representative rep where
	{
		-- | Every value is an instance of 'Is'.
		;
		getRepWitness :: rep a -> RepWitness rep a;
	};

	withRepresentative :: forall rep r. (Representative rep) => (forall a. (Is rep a) => rep a -> r) -> (forall b. rep b -> r);
	withRepresentative foo rep = case getRepWitness rep of
	{
		MkRepWitness -> foo rep;
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

	instance Eq1 (RepWitness rep) where
	{
		equals1 MkRepWitness MkRepWitness = True;
	};

	instance Representative (RepWitness rep) where
	{
		getRepWitness MkRepWitness = MkRepWitness;
	};

	instance (Is rep a) => Is (RepWitness rep) a where
	{
		representative = MkRepWitness;
	};
}
