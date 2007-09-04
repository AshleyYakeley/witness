module Data.Witness.Witness where
{
	import Data.Witness.AllMap;
	import Data.Witness.Type;
	import Data.Witness.Representative;

	-- every witness has a type
	class Witness w where
	{
		matchWitnessF :: w a -> w a' -> Maybe (AllMap a a');
	};

	matchIsF :: (Witness w,Is w a,Is w a') => Type (w ()) -> Maybe (AllMap a a');
	matchIsF t = matchWitnessF (foo t) (foo t) where
	{
		foo :: (Is w a) => Type (w ()) -> w a;
		foo _ = representative;
	};

	matchWitness :: (Witness w) => w a -> w a' -> Maybe (a -> a');
	matchWitness wa wb = matchWitnessF wa wb >>= (\mapF -> return (mapID mapF));

	matchIs :: (Witness w,Is w a,Is w a') => Type (w ()) -> Maybe (a -> a');
	matchIs t = matchIsF t >>= (\mapF -> return (mapID mapF));
}
