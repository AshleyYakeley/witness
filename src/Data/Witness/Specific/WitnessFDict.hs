module Data.Witness.Specific.WitnessFDict where

import Data.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessFDict :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WitnessFDict w f =
    MkWitnessFDict [SomeFor w f]
    deriving (Semigroup, Monoid)

-- | An empty dictionary.
emptyWitnessFDict :: WitnessFDict w f
emptyWitnessFDict = mempty

-- | Look up the first value in the dictionary that matches the given witness.
witnessFDictLookup :: (TestEquality w) => w a -> WitnessFDict w f -> Maybe (f a)
witnessFDictLookup wit (MkWitnessFDict cells) = listToMaybe (mapMaybe (matchSomeFor wit) cells)

-- | Modify the first value in the dictionary that matches a particular witness.
witnessFDictModify :: (TestEquality w) => w a -> (f a -> f a) -> WitnessFDict w f -> WitnessFDict w f
witnessFDictModify wit amap (MkWitnessFDict cells) =
    MkWitnessFDict (replaceFirst ((fmap ((MkSomeFor wit) . amap)) . (matchSomeFor wit)) cells)
  where
    replaceFirst :: (a -> Maybe a) -> [a] -> [a]
    replaceFirst ama (a:aa) =
        case ama a of
            Just newa -> (newa : aa)
            _ -> a : (replaceFirst ama aa)
    replaceFirst _ _ = []

-- | Replace the first value in the dictionary that matches the witness
witnessFDictReplace :: (TestEquality w) => w a -> f a -> WitnessFDict w f -> WitnessFDict w f
witnessFDictReplace wit newfa = witnessFDictModify wit (const newfa)

-- | Add a witness and value as the first entry in the dictionary.
witnessFDictAdd :: w a -> f a -> WitnessFDict w f -> WitnessFDict w f
witnessFDictAdd wit fa (MkWitnessFDict cells) = MkWitnessFDict ((MkSomeFor wit fa) : cells)

-- | A dictionary for a single witness and value
witnessFDictSingle :: w a -> f a -> WitnessFDict w f
witnessFDictSingle wit fa = MkWitnessFDict $ pure $ MkSomeFor wit fa

-- | Remove the first entry in the dictionary that matches the given witness.
witnessFDictRemove :: (TestEquality w) => w a -> WitnessFDict w f -> WitnessFDict w f
witnessFDictRemove wit (MkWitnessFDict cells) =
    MkWitnessFDict (removeFirst (\(MkSomeFor cwit _) -> isJust (testEquality wit cwit)) cells)
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst p (a:as)
        | p a = as
    removeFirst p (a:as) = a : (removeFirst p as)
    removeFirst _ _ = []

-- | Create a dictionary from a list of witness\/value pairs
witnessFDictFromList :: [SomeFor w f] -> WitnessFDict w f
witnessFDictFromList = MkWitnessFDict

witnessFDictMapM :: Applicative m => (forall a. f a -> m (g a)) -> WitnessFDict w f -> m (WitnessFDict w g)
witnessFDictMapM fmg (MkWitnessFDict cells) =
    fmap MkWitnessFDict $ for cells $ \(MkSomeFor wit fa) -> fmap (MkSomeFor wit) $ fmg fa