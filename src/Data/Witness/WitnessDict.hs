module Data.Witness.WitnessDict where

import Data.Witness.Any
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessDict :: (Type -> Type) -> Type
newtype WitnessDict w =
    MkWitnessDict [AnyValue w]
    deriving (Semigroup, Monoid)

-- | An empty dictionary.
emptyWitnessDict :: WitnessDict w
emptyWitnessDict = mempty

-- | Look up the first value in the dictionary that matches the given witness.
witnessDictLookup :: (TestEquality w) => w a -> WitnessDict w -> Maybe a
witnessDictLookup wit (MkWitnessDict cells) = listToMaybe (mapMaybe (matchAnyValue wit) cells)

-- | Modify the first value in the dictionary that matches a particular witness.
witnessDictModify :: (TestEquality w) => w a -> (a -> a) -> WitnessDict w -> WitnessDict w
witnessDictModify wit amap (MkWitnessDict cells) =
    MkWitnessDict (replaceFirst ((fmap ((MkAnyValue wit) . amap)) . (matchAnyValue wit)) cells)
  where
    replaceFirst :: (a -> Maybe a) -> [a] -> [a]
    replaceFirst ama (a:aa) =
        case ama a of
            Just newa -> (newa : aa)
            _ -> a : (replaceFirst ama aa)
    replaceFirst _ _ = []

-- | Replace the first value in the dictionary that matches the witness
witnessDictReplace :: (TestEquality w) => w a -> a -> WitnessDict w -> WitnessDict w
witnessDictReplace wit newa = witnessDictModify wit (const newa)

witnessDictSingle :: w a -> a -> WitnessDict w
witnessDictSingle wit a = MkWitnessDict $ pure $ MkAnyValue wit a

witnessDictFold :: Monoid m => WitnessDict w -> (forall a. w a -> a -> m) -> m
witnessDictFold (MkWitnessDict cells) f = mconcat $ fmap (\(MkAnyValue wit a) -> f wit a) cells

-- | Add a witness and value as the first entry in the dictionary.
witnessDictAdd :: w a -> a -> WitnessDict w -> WitnessDict w
witnessDictAdd wit a (MkWitnessDict cells) = MkWitnessDict $ (MkAnyValue wit a) : cells

-- | Remove the first entry in the dictionary that matches the given witness.
witnessDictRemove :: (TestEquality w) => w a -> WitnessDict w -> WitnessDict w
witnessDictRemove wit (MkWitnessDict cells) =
    MkWitnessDict (removeFirst (\(MkAnyValue cwit _) -> isJust (testEquality wit cwit)) cells)
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst p (a:as)
        | p a = as
    removeFirst p (a:as) = a : (removeFirst p as)
    removeFirst _ _ = []

-- | Create a dictionary from a list of witness\/value pairs
witnessDictFromList :: [AnyValue w] -> WitnessDict w
witnessDictFromList = MkWitnessDict
