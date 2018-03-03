module Data.Witness.ListElement where

import Data.Countable
import Data.Empty
import Data.Maybe
import Data.Searchable
import Data.Type.Equality
import Prelude

data ListElementWitness (kk :: [k]) (t :: k) where
    FirstListElementWitness :: ListElementWitness (t : tt) t
    RestListElementWitness :: ListElementWitness aa t -> ListElementWitness (a : aa) t

instance TestEquality (ListElementWitness tt) where
    testEquality FirstListElementWitness FirstListElementWitness = Just Refl
    testEquality (RestListElementWitness lt1) (RestListElementWitness lt2) = do
        Refl <- testEquality lt1 lt2
        return Refl
    testEquality _ _ = Nothing

instance Searchable (ListElementWitness '[] t) where
    search = finiteSearch

instance Eq (ListElementWitness tt t) where
    lt1 == lt2 = isJust $ testEquality lt1 lt2

instance Countable (ListElementWitness '[] t) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Finite (ListElementWitness '[] t) where
    allValues = []

instance Empty (ListElementWitness '[] t) where
    never lt = case lt of {}
