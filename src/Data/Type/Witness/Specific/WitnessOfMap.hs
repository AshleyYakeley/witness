module Data.Type.Witness.Specific.WitnessOfMap where

import Data.Type.Witness.Specific.Some
import Data.Type.Witness.Specific.WitnessForMap
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessOfMap :: (Type -> Type) -> Type
type WitnessOfMap = WitnessForMap Identity

-- | An empty dictionary.
emptyWitnessOfMap :: WitnessOfMap w
emptyWitnessOfMap = emptyWitnessForMap

-- | Look up the first value in the dictionary that matches the given witness.
witnessOfMapLookup :: (TestEquality w) => w a -> WitnessOfMap w -> Maybe a
witnessOfMapLookup wit d = fmap runIdentity $ witnessForMapLookup wit d

-- | Modify the first value in the dictionary that matches a particular witness.
witnessOfMapModify :: (TestEquality w) => w a -> (a -> a) -> WitnessOfMap w -> WitnessOfMap w
witnessOfMapModify wit amap = witnessForMapModify wit (Identity . amap . runIdentity)

-- | Replace the first value in the dictionary that matches the witness
witnessOfMapReplace :: (TestEquality w) => w a -> a -> WitnessOfMap w -> WitnessOfMap w
witnessOfMapReplace wit newa = witnessOfMapModify wit $ const newa

witnessOfMapSingle :: w a -> a -> WitnessOfMap w
witnessOfMapSingle wit a = witnessForMapSingle wit $ Identity a

witnessOfMapFold :: Monoid m => WitnessOfMap w -> (forall a. w a -> a -> m) -> m
witnessOfMapFold d f = witnessForMapFold d $ \wa (Identity a) -> f wa a

-- | Add a witness and value as the first entry in the dictionary.
witnessOfMapAdd :: w a -> a -> WitnessOfMap w -> WitnessOfMap w
witnessOfMapAdd wit a = witnessForMapAdd wit $ Identity a

-- | Remove the first entry in the dictionary that matches the given witness.
witnessOfMapRemove :: (TestEquality w) => w a -> WitnessOfMap w -> WitnessOfMap w
witnessOfMapRemove = witnessForMapRemove

-- | Create a dictionary from a list of witness\/value pairs
witnessOfMapFromList :: [SomeOf w] -> WitnessOfMap w
witnessOfMapFromList = MkWitnessForMap
