module Data.Witness.Specific.WitnessForDict where

import Data.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessForDict :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WitnessForDict w f =
    MkWitnessForDict [SomeFor w f]
    deriving (Semigroup, Monoid)

-- | An empty dictionary.
emptyWitnessForDict :: WitnessForDict w f
emptyWitnessForDict = mempty

-- | Look up the first value in the dictionary that matches the given witness.
witnessForDictLookup :: (TestEquality w) => w a -> WitnessForDict w f -> Maybe (f a)
witnessForDictLookup wit (MkWitnessForDict cells) = listToMaybe (mapMaybe (matchSomeFor wit) cells)

-- | Modify the first value in the dictionary that matches a particular witness.
witnessForDictModify :: (TestEquality w) => w a -> (f a -> f a) -> WitnessForDict w f -> WitnessForDict w f
witnessForDictModify wit amap (MkWitnessForDict cells) =
    MkWitnessForDict (replaceFirst ((fmap ((MkSomeFor wit) . amap)) . (matchSomeFor wit)) cells)
  where
    replaceFirst :: (a -> Maybe a) -> [a] -> [a]
    replaceFirst ama (a:aa) =
        case ama a of
            Just newa -> (newa : aa)
            _ -> a : (replaceFirst ama aa)
    replaceFirst _ _ = []

-- | Replace the first value in the dictionary that matches the witness
witnessForDictReplace :: (TestEquality w) => w a -> f a -> WitnessForDict w f -> WitnessForDict w f
witnessForDictReplace wit newfa = witnessForDictModify wit (const newfa)

-- | Add a witness and value as the first entry in the dictionary.
witnessForDictAdd :: w a -> f a -> WitnessForDict w f -> WitnessForDict w f
witnessForDictAdd wit fa (MkWitnessForDict cells) = MkWitnessForDict ((MkSomeFor wit fa) : cells)

-- | A dictionary for a single witness and value
witnessForDictSingle :: w a -> f a -> WitnessForDict w f
witnessForDictSingle wit fa = MkWitnessForDict $ pure $ MkSomeFor wit fa

witnessForDictFold :: Monoid m => WitnessForDict w f -> (forall a. w a -> f a -> m) -> m
witnessForDictFold (MkWitnessForDict cells) f = mconcat $ fmap (\(MkSomeFor wit fa) -> f wit fa) cells

-- | Remove the first entry in the dictionary that matches the given witness.
witnessForDictRemove :: (TestEquality w) => w a -> WitnessForDict w f -> WitnessForDict w f
witnessForDictRemove wit (MkWitnessForDict cells) =
    MkWitnessForDict (removeFirst (\(MkSomeFor cwit _) -> isJust (testEquality wit cwit)) cells)
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst p (a:as)
        | p a = as
    removeFirst p (a:as) = a : (removeFirst p as)
    removeFirst _ _ = []

-- | Create a dictionary from a list of witness\/value pairs
witnessForDictFromList :: [SomeFor w f] -> WitnessForDict w f
witnessForDictFromList = MkWitnessForDict

witnessForDictMapM :: Applicative m => (forall a. f a -> m (g a)) -> WitnessForDict w f -> m (WitnessForDict w g)
witnessForDictMapM fmg (MkWitnessForDict cells) =
    fmap MkWitnessForDict $ for cells $ \(MkSomeFor wit fa) -> fmap (MkSomeFor wit) $ fmg fa
