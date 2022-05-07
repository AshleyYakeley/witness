module Data.Type.Witness.Specific.WitnessForMap where

import Data.Type.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessForMap :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WitnessForMap f w =
    MkWitnessForMap [SomeFor f w]
    deriving (Semigroup, Monoid)

-- | An empty dictionary.
emptyWitnessForMap :: WitnessForMap f w
emptyWitnessForMap = mempty

-- | Look up the first value in the dictionary that matches the given witness.
witnessForMapLookup :: (TestEquality w) => w a -> WitnessForMap f w -> Maybe (f a)
witnessForMapLookup wit (MkWitnessForMap cells) = listToMaybe (mapMaybe (matchSomeFor wit) cells)

-- | Modify the first value in the dictionary that matches a particular witness.
witnessForMapModify :: (TestEquality w) => w a -> (f a -> f a) -> WitnessForMap f w -> WitnessForMap f w
witnessForMapModify wit amap (MkWitnessForMap cells) =
    MkWitnessForMap (replaceFirst ((fmap ((MkSomeFor wit) . amap)) . (matchSomeFor wit)) cells)
  where
    replaceFirst :: (a -> Maybe a) -> [a] -> [a]
    replaceFirst ama (a:aa) =
        case ama a of
            Just newa -> (newa : aa)
            _ -> a : (replaceFirst ama aa)
    replaceFirst _ _ = []

-- | Replace the first value in the dictionary that matches the witness
witnessForMapReplace :: (TestEquality w) => w a -> f a -> WitnessForMap f w -> WitnessForMap f w
witnessForMapReplace wit newfa = witnessForMapModify wit (const newfa)

-- | Add a witness and value as the first entry in the dictionary.
witnessForMapAdd :: w a -> f a -> WitnessForMap f w -> WitnessForMap f w
witnessForMapAdd wit fa (MkWitnessForMap cells) = MkWitnessForMap ((MkSomeFor wit fa) : cells)

-- | A dictionary for a single witness and value
witnessForMapSingle :: w a -> f a -> WitnessForMap f w
witnessForMapSingle wit fa = MkWitnessForMap $ pure $ MkSomeFor wit fa

witnessForMapFold :: Monoid m => WitnessForMap f w -> (forall a. w a -> f a -> m) -> m
witnessForMapFold (MkWitnessForMap cells) f = mconcat $ fmap (\(MkSomeFor wit fa) -> f wit fa) cells

-- | Remove the first entry in the dictionary that matches the given witness.
witnessForMapRemove :: (TestEquality w) => w a -> WitnessForMap f w -> WitnessForMap f w
witnessForMapRemove wit (MkWitnessForMap cells) =
    MkWitnessForMap (removeFirst (\(MkSomeFor cwit _) -> isJust (testEquality wit cwit)) cells)
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst p (a:as)
        | p a = as
    removeFirst p (a:as) = a : (removeFirst p as)
    removeFirst _ _ = []

-- | Create a dictionary from a list of witness\/value pairs
witnessForMapFromList :: [SomeFor f w] -> WitnessForMap f w
witnessForMapFromList = MkWitnessForMap

witnessForMapMapM :: Applicative m => (forall a. f a -> m (g a)) -> WitnessForMap f w -> m (WitnessForMap g w)
witnessForMapMapM fmg (MkWitnessForMap cells) =
    fmap MkWitnessForMap $ for cells $ \(MkSomeFor wit fa) -> fmap (MkSomeFor wit) $ fmg fa
