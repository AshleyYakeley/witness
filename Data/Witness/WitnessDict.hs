module Data.Witness.WitnessDict
(
	WitnessDict,emptyWitnessDict,
	witnessDictLookup,witnessDictReplace,witnessDictModify,witnessDictAdd,witnessDictFromList
) where
{
	import Data.Witness;
	import Control.Monad.State;
	
	newtype WitnessDict w = MkWitnessDict [Any w];
	
	emptyWitnessDict :: WitnessDict w;
	emptyWitnessDict = MkWitnessDict[];
	
	witnessDictLookup :: (Witness w) => w a -> WitnessDict w -> Maybe a;
	witnessDictLookup wit (MkWitnessDict cells) = findM (\(MkAny cwit ca) -> do
	{
		MkSameType <- matchWitness cwit wit;
		return ca;
	}) cells where
	{
		findM :: (a -> Maybe b) -> [a] -> Maybe b;
		findM match (a:as) = case match a of
		{
			mb@(Just _) -> mb;
			_ -> findM match as;
		};
		findM _ _ = Nothing;
	};
	
	witnessDictModify :: (Witness w) => w a -> (a -> a) -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictModify wit amap (MkWitnessDict cc) = fmap MkWitnessDict (replaceFirst (mapCell wit amap) cc) where
	{
		replaceFirst :: (a -> Maybe a) -> [a] -> Maybe [a];
		replaceFirst ama (a:aa) = case ama a of
		{
			Just newa -> Just (newa:aa);
			_ -> fmap (a :) (replaceFirst ama aa);
		};
		replaceFirst _ _ = Nothing;
		
		mapCell :: (Witness w) => w a -> (a -> a) -> Any w -> Maybe (Any w);
		mapCell wit' amap' (MkAny w a') = mapAny (\cwit a -> do
		{
			MkSameType <- matchWitness wit' cwit;
			return (amap' a);
		}) (MkAny w a');
		
		mapAny :: (forall a. w a -> a -> Maybe a) -> Any w -> Maybe (Any w);
		mapAny wama (MkAny w a) = fmap (MkAny w) (wama w a);
	};

	witnessDictReplace :: (Witness w) => w a -> a -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictReplace wit newa = witnessDictModify wit (const newa);
	
	witnessDictAdd :: w a -> a -> WitnessDict w -> WitnessDict w;
	witnessDictAdd wit a (MkWitnessDict cells) = MkWitnessDict ((MkAny wit a):cells);
	
	witnessDictFromList :: (Witness w) => [Any w] -> WitnessDict w;
	witnessDictFromList = MkWitnessDict;
}
