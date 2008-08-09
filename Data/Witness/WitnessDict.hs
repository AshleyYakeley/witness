module Data.Witness.WitnessDict where
{
	import Data.Witness;
	import Control.Monad.State;
	import Data.Maybe;
	
	newtype WitnessDict w = MkWitnessDict [Any w];
	
	emptyWitnessDict :: WitnessDict w;
	emptyWitnessDict = MkWitnessDict[];
	
	witnessDictLookup :: (SimpleWitness w) => w a -> WitnessDict w -> Maybe a;
	witnessDictLookup wit (MkWitnessDict cells) = listToMaybe (mapMaybe (matchAny wit) cells);
	
	witnessDictModify :: (SimpleWitness w) => w a -> (a -> a) -> WitnessDict w -> WitnessDict w;
	witnessDictModify wit amap (MkWitnessDict cells) = MkWitnessDict 
		(replaceFirst ((fmap ((MkAny wit) . amap)) . (matchAny wit)) cells) where
	{
		replaceFirst :: (a -> Maybe a) -> [a] -> [a];
		replaceFirst ama (a:aa) = case ama a of
		{
			Just newa -> (newa:aa);
			_ -> a : (replaceFirst ama aa);
		};
		replaceFirst _ _ = [];
	};

	witnessDictReplace :: (SimpleWitness w) => w a -> a -> WitnessDict w -> WitnessDict w;
	witnessDictReplace wit newa = witnessDictModify wit (const newa);
	
	witnessDictAdd :: w a -> a -> WitnessDict w -> WitnessDict w;
	witnessDictAdd wit a (MkWitnessDict cells) = MkWitnessDict ((MkAny wit a):cells);
	
	witnessDictRemove :: (SimpleWitness w) => w a -> WitnessDict w -> WitnessDict w;
	witnessDictRemove wit (MkWitnessDict cells) = MkWitnessDict 
		(removeFirst (\(MkAny cwit _) -> isJust (matchWitness wit cwit)) cells) where
	{
		removeFirst :: (a -> Bool) -> [a] -> [a];
		removeFirst p (a:as) | p a = as;
		removeFirst p (a:as) = a : (removeFirst p as);
		removeFirst _ _ = [];
	};
	
	witnessDictFromList :: (SimpleWitness w) => [Any w] -> WitnessDict w;
	witnessDictFromList = MkWitnessDict;
}
