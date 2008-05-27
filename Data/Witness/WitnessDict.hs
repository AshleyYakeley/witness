module Data.Witness.WitnessDict where
{
	import Data.Witness;
	import Control.Monad.State;
	import Data.Maybe;
	
	newtype WitnessDict w = MkWitnessDict [Any w];
	
	emptyWitnessDict :: WitnessDict w;
	emptyWitnessDict = MkWitnessDict[];
	
	witnessDictLookup :: (Witness w) => w a -> WitnessDict w -> Maybe a;
	witnessDictLookup wit (MkWitnessDict cells) = listToMaybe (mapMaybe (matchAny wit) cells);
	
	witnessDictModify :: (Witness w) => w a -> (a -> a) -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictModify wit amap (MkWitnessDict cells) = fmap MkWitnessDict 
		(replaceFirst ((fmap ((MkAny wit) . amap)) . (matchAny wit)) cells) where
	{
		replaceFirst :: (a -> Maybe a) -> [a] -> Maybe [a];
		replaceFirst ama (a:aa) = case ama a of
		{
			Just newa -> Just (newa:aa);
			_ -> fmap (a :) (replaceFirst ama aa);
		};
		replaceFirst _ _ = Nothing;
	};

	witnessDictReplace :: (Witness w) => w a -> a -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictReplace wit newa = witnessDictModify wit (const newa);
	
	witnessDictAdd :: w a -> a -> WitnessDict w -> WitnessDict w;
	witnessDictAdd wit a (MkWitnessDict cells) = MkWitnessDict ((MkAny wit a):cells);
	
	witnessDictRemove :: (Witness w) => w a -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictRemove wit (MkWitnessDict cells) = fmap MkWitnessDict 
		(removeFirst (\(MkAny cwit _) -> isJust (matchWitness wit cwit)) cells) where
	{
		removeFirst :: (a -> Bool) -> [a] -> Maybe [a];
		removeFirst p (a:as) | p a = Just as;
		removeFirst p (a:as) = fmap (a :) (removeFirst p as);
		removeFirst _ _ = Nothing;
	};
	
	witnessDictFromList :: (Witness w) => [Any w] -> WitnessDict w;
	witnessDictFromList = MkWitnessDict;
}
