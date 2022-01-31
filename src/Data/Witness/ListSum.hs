module Data.Witness.ListSum where

import Data.Constraint (Dict(..))
import Data.Kind
import Data.Type.Equality
import Data.Void
import Data.Witness.Constraint
import Data.Witness.List
import Prelude hiding ((.), id)
import Unsafe.Coerce

type ListSum :: [Type] -> Type
type family ListSum w = r | r -> w where
    ListSum '[] = Void
    ListSum (t : tt) = Either t (ListSum tt)

-- workaround for https://gitlab.haskell.org/ghc/ghc/issues/10833
injectiveListSum ::
       forall (a :: [Type]) (b :: [Type]). ListSum a ~ ListSum b
    => a :~: b
injectiveListSum = unsafeCoerce Refl

listSumEq :: (forall a. w a -> Dict (Eq a)) -> ListType w t -> Dict (Eq (ListSum t))
listSumEq _ NilListType = Dict
listSumEq f (ConsListType t tt) =
    case (f t, listSumEq f tt) of
        (Dict, Dict) -> Dict

listSumShow :: (forall a. w a -> Dict (Show a)) -> ListType w t -> Dict (Show (ListSum t))
listSumShow _ NilListType = Dict
listSumShow f (ConsListType t tt) =
    case (f t, listSumShow f tt) of
        (Dict, Dict) -> Dict

type ListSumWit :: (Type -> Type) -> (Type -> Type)
data ListSumWit wit t where
    MkListSumWit :: forall (wit :: Type -> Type) (lt :: [Type]). ListType wit lt -> ListSumWit wit (ListSum lt)

instance WitnessConstraint Eq w => WitnessConstraint Eq (ListSumWit w) where
    witnessConstraint (MkListSumWit lt) = listSumEq witnessConstraint lt
