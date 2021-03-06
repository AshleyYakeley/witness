module Data.Witness.List where

import Data.Constraint (Dict(..))
import Data.Functor.Identity
import Data.Kind
import Data.List (intercalate)
import Data.Nat
import Data.Type.Equality
import Data.Witness.Any
import Data.Witness.Representative
import Prelude hiding ((.), id)

-- | a witness type for lists of types
-- The @w@ parameter is the witness type of the elements.
data ListType (w :: k -> Type) (lt :: [k]) where
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

assembleListType :: [AnyW w] -> AnyW (ListType w)
assembleListType [] = MkAnyW NilListType
assembleListType ((MkAnyW wa):ww) =
    case assembleListType ww of
        MkAnyW wwa -> MkAnyW $ ConsListType wa wwa

mapMListType :: Applicative m => (forall t'. wita t' -> m (witb t')) -> ListType wita t -> m (ListType witb t)
mapMListType _ff NilListType = pure NilListType
mapMListType ff (ConsListType t tt) = ConsListType <$> ff t <*> mapMListType ff tt

mapListType :: (forall t'. wita t' -> witb t') -> ListType wita t -> ListType witb t
mapListType ff l = runIdentity $ mapMListType (\wt -> Identity $ ff wt) l

type family ListElement (n :: Nat) (list :: [k]) :: k where
    ListElement 'Zero (a : aa) = a
    ListElement ('Succ n) (a : aa) = ListElement n aa

listTypeLength :: ListType w lt -> Int
listTypeLength NilListType = 0
listTypeLength (ConsListType _ lt) = succ $ listTypeLength lt

listTypeToList :: (forall a. w a -> r) -> ListType w t -> [r]
listTypeToList _wr NilListType = []
listTypeToList wr (ConsListType wa rest) = (wr wa) : (listTypeToList wr rest)

listTypeFor_ :: Applicative m => ListType w t -> (forall a. w a -> m ()) -> m ()
listTypeFor_ NilListType _ = pure ()
listTypeFor_ (ConsListType t tt) f = f t *> listTypeFor_ tt f
