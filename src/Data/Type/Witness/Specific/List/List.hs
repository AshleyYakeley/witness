module Data.Type.Witness.Specific.List.List where

import Data.Type.Witness.General.Representative
import Data.Type.Witness.Specific.Pair
import Data.Type.Witness.Specific.Some
import Import

-- | a witness type for lists of types
-- The @w@ parameter is the witness type of the elements.
type ListType :: (k -> Type) -> ([k] -> Type)
data ListType w lt where
    NilListType :: ListType w '[]
    ConsListType :: w a -> ListType w lt -> ListType w (a : lt)

instance Representative w => Representative (ListType w) where
    getRepWitness NilListType = Dict
    getRepWitness (ConsListType w lw) =
        case (getRepWitness w, getRepWitness lw) of
            (Dict, Dict) -> Dict

instance Representative w => Is (ListType w) '[] where
    representative = NilListType

instance (Is w a, Is (ListType w) lt) => Is (ListType w) (a : lt) where
    representative = ConsListType representative representative

instance TestEquality w => TestEquality (ListType w) where
    testEquality NilListType NilListType = Just Refl
    testEquality (ConsListType wpa wpb) (ConsListType wqa wqb) = do
        Refl <- testEquality wpa wqa
        Refl <- testEquality wpb wqb
        return Refl
    testEquality _ _ = Nothing

instance (forall a. Show (w a)) => Show (ListType w lt) where
    show s = "[" <> intercalate "," (showAll s) <> "]"
      where
        showAll :: forall t. ListType w t -> [String]
        showAll NilListType = []
        showAll (ConsListType t1 tr) = show t1 : showAll tr

assembleListType :: [Some w] -> Some (ListType w)
assembleListType [] = MkSome NilListType
assembleListType ((MkSome wa):ww) =
    case assembleListType ww of
        MkSome wwa -> MkSome $ ConsListType wa wwa

mapMListType :: Applicative m => (forall t'. wita t' -> m (witb t')) -> ListType wita t -> m (ListType witb t)
mapMListType _ff NilListType = pure NilListType
mapMListType ff (ConsListType t tt) = ConsListType <$> ff t <*> mapMListType ff tt

joinMListType ::
       Applicative m
    => (forall t'. wita t' -> witb t' -> m (witc t'))
    -> ListType wita t
    -> ListType witb t
    -> m (ListType witc t)
joinMListType _ff NilListType NilListType = pure NilListType
joinMListType ff (ConsListType t1 t1t) (ConsListType t2 t2t) = ConsListType <$> ff t1 t2 <*> joinMListType ff t1t t2t

mapListType :: (forall t'. wita t' -> witb t') -> ListType wita t -> ListType witb t
mapListType ff l = runIdentity $ mapMListType (\wt -> Identity $ ff wt) l

joinListType :: (forall t'. wita t' -> witb t' -> witc t') -> ListType wita t -> ListType witb t -> ListType witc t
joinListType ff la lb = runIdentity $ joinMListType (\wta wtb -> Identity $ ff wta wtb) la lb

pairListType :: ListType wita t -> ListType witb t -> ListType (PairType wita witb) t
pairListType = joinListType MkPairType

listTypeLength :: ListType w lt -> Int
listTypeLength NilListType = 0
listTypeLength (ConsListType _ lt) = succ $ listTypeLength lt

listTypeToList :: (forall a. w a -> r) -> ListType w t -> [r]
listTypeToList _wr NilListType = []
listTypeToList wr (ConsListType wa rest) = (wr wa) : (listTypeToList wr rest)

listTypeFor :: Applicative m => ListType w t -> (forall a. w a -> m r) -> m [r]
listTypeFor NilListType _ = pure []
listTypeFor (ConsListType t tt) f = liftA2 (:) (f t) $ listTypeFor tt f

listTypeFor_ :: Applicative m => ListType w t -> (forall a. w a -> m ()) -> m ()
listTypeFor_ NilListType _ = pure ()
listTypeFor_ (ConsListType t tt) f = f t *> listTypeFor_ tt f
