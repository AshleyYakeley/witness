module Data.Witness.WitnessDict
(
	WitnessDict,emptyWitnessDict,
	witnessDictLookup,witnessDictReplace,witnessDictModify,witnessDictAdd
) where
{
	import Data.Witness;
	import Control.Monad.State;
	
	newtype WitnessDict w = MkWitnessDict [Any w];
	
	emptyWitnessDict :: WitnessDict w;
	emptyWitnessDict = MkWitnessDict[];
	
	findM :: (a -> Maybe b) -> [a] -> Maybe b;
	findM match (a:as) = case match a of
	{
		mb@(Just _) -> mb;
		_ -> findM match as;
	};
	findM _ _ = Nothing;
	
	witnessDictLookup :: (Witness w) => w a -> WitnessDict w -> Maybe a;
	witnessDictLookup wit (MkWitnessDict cells) = findM (\(MkAny cwit ca) -> do
	{
		MkSameType <- matchWitness cwit wit;
		return ca;
	}) cells;
	
	replaceHelper :: (forall a. w a -> Maybe (a -> a)) -> Any w -> Maybe (Any w);
	replaceHelper wmaa (MkAny wit a) = case wmaa wit of
	{
		Just aa -> Just $ MkAny wit (aa a);
		_ -> Nothing;
	};

	witnessDictModify :: (Witness w) => w a -> (a -> a) -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictModify wit amap (MkWitnessDict cc) = fmap MkWitnessDict (mapCells cc) where
	{
		mapCells (cell:cells) = case replaceHelper (\cwit -> do
		{
			MkSameType <- matchWitness wit cwit;
			return amap;
		}) cell of
		{
			Just newcell -> Just (newcell:cells);
			_ -> fmap (cell :) (mapCells cells);
		};
		mapCells _ = Nothing;
	};

	witnessDictReplace :: (Witness w) => w a -> a -> WitnessDict w -> Maybe (WitnessDict w);
	witnessDictReplace wit newa = witnessDictModify wit (const newa);
	
	witnessDictAdd :: w a -> a -> WitnessDict w -> WitnessDict w;
	witnessDictAdd wit a (MkWitnessDict cells) = MkWitnessDict ((MkAny wit a):cells);
}
