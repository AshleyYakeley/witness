module Data.Witness.ListElement where

import Data.Countable
import Data.Empty
import Data.Maybe
import Data.Searchable
import Data.Type.Equality
import Prelude

data ListElementType (kk :: [k]) (t :: k) where
    FirstElementType :: ListElementType (t : tt) t
    RestElementType :: ListElementType aa t -> ListElementType (a : aa) t

instance TestEquality (ListElementType tt) where
    testEquality FirstElementType FirstElementType = Just Refl
    testEquality (RestElementType lt1) (RestElementType lt2) = do
        Refl <- testEquality lt1 lt2
        return Refl
    testEquality _ _ = Nothing

instance Searchable (ListElementType '[] t) where
    search = finiteSearch

instance Eq (ListElementType tt t) where
    lt1 == lt2 = isJust $ testEquality lt1 lt2

instance Countable (ListElementType '[] t) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Finite (ListElementType '[] t) where
    allValues = []

instance Empty (ListElementType '[] t) where
    never lt = case lt of {}
