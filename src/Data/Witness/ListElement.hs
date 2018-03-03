module Data.Witness.ListElement where

import Data.Countable
import Data.Empty
import Data.Maybe
import Data.Searchable
import Data.Type.Equality

data ListThingWitness (kk :: [k]) (t :: k) where
    FirstListThingWitness :: ListThingWitness (t : tt) t
    RestListThingWitness :: ListThingWitness aa t -> ListThingWitness (a : aa) t

instance TestEquality (ListThingWitness tt) where
    testEquality FirstListThingWitness FirstListThingWitness = Just Refl
    testEquality (RestListThingWitness lt1) (RestListThingWitness lt2) = do
        Refl <- testEquality lt1 lt2
        return Refl
    testEquality _ _ = Nothing

instance Searchable (ListThingWitness '[] t) where
    search = finiteSearch

instance Eq (ListThingWitness tt t) where
    lt1 == lt2 = isJust $ testEquality lt1 lt2

instance Countable (ListThingWitness '[] t) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Finite (ListThingWitness '[] t) where
    allValues = []

instance Empty (ListThingWitness '[] t) where
    never lt = case lt of {}
