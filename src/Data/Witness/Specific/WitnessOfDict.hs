module Data.Witness.Specific.WitnessOfDict where

import Data.Witness.Specific.Some
import Data.Witness.Specific.WitnessForDict
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessOfDict :: (Type -> Type) -> Type
type WitnessOfDict = WitnessForDict Identity

-- | An empty dictionary.
emptyWitnessOfDict :: WitnessOfDict w
emptyWitnessOfDict = emptyWitnessForDict

-- | Look up the first value in the dictionary that matches the given witness.
witnessOfDictLookup :: (TestEquality w) => w a -> WitnessOfDict w -> Maybe a
witnessOfDictLookup wit d = fmap runIdentity $ witnessForDictLookup wit d

-- | Modify the first value in the dictionary that matches a particular witness.
witnessOfDictModify :: (TestEquality w) => w a -> (a -> a) -> WitnessOfDict w -> WitnessOfDict w
witnessOfDictModify wit amap = witnessForDictModify wit (Identity . amap . runIdentity)

-- | Replace the first value in the dictionary that matches the witness
witnessOfDictReplace :: (TestEquality w) => w a -> a -> WitnessOfDict w -> WitnessOfDict w
witnessOfDictReplace wit newa = witnessOfDictModify wit $ const newa

witnessOfDictSingle :: w a -> a -> WitnessOfDict w
witnessOfDictSingle wit a = witnessForDictSingle wit $ Identity a

witnessOfDictFold :: Monoid m => WitnessOfDict w -> (forall a. w a -> a -> m) -> m
witnessOfDictFold d f = witnessForDictFold d $ \wa (Identity a) -> f wa a

-- | Add a witness and value as the first entry in the dictionary.
witnessOfDictAdd :: w a -> a -> WitnessOfDict w -> WitnessOfDict w
witnessOfDictAdd wit a = witnessForDictAdd wit $ Identity a

-- | Remove the first entry in the dictionary that matches the given witness.
witnessOfDictRemove :: (TestEquality w) => w a -> WitnessOfDict w -> WitnessOfDict w
witnessOfDictRemove = witnessForDictRemove

-- | Create a dictionary from a list of witness\/value pairs
witnessOfDictFromList :: [SomeOf w] -> WitnessOfDict w
witnessOfDictFromList = MkWitnessForDict
