module Data.Witness.ListElement where

import Data.Countable
import Data.Empty
import Data.Kind
import Data.Maybe
import Data.Searchable
import Data.Type.Equality
import Data.Witness.List
import Prelude

type ListElementType :: forall k. [k] -> k -> Type
data ListElementType kk t where
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

lookUpListElement ::
       forall k (w :: k -> Type) (t :: k) (lt :: [k]). TestEquality w
    => w t
    -> ListType w lt
    -> Maybe (ListElementType lt t)
lookUpListElement _ NilListType = Nothing
lookUpListElement wt (ConsListType wt' _)
    | Just Refl <- testEquality wt wt' = Just FirstElementType
lookUpListElement wt (ConsListType _ lt) = do
    et <- lookUpListElement wt lt
    return $ RestElementType et

countListType :: ListType w lt -> ListType (ListElementType lt) lt
countListType NilListType = NilListType
countListType (ConsListType _ lt) = ConsListType FirstElementType (mapListType RestElementType $ countListType lt)
