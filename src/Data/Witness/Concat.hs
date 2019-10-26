module Data.Witness.Concat where

import Data.Constraint (Dict(..))
import Data.Kind
import Data.Type.Equality
import Data.Witness.List
import Data.Witness.Representative
import Prelude hiding ((.), id)

type family Concat (a :: [k]) (b :: [k]) :: [k] where
    Concat '[] bb = bb
    Concat (a ': aa) bb = a ': (Concat aa bb)

concatEmptyRefl :: ListType w a -> Concat a '[] :~: a
concatEmptyRefl NilListType = Refl
concatEmptyRefl (ConsListType _ la) =
    case concatEmptyRefl la of
        Refl -> Refl

concatIsDict ::
       forall w aa bb. (Representative w, Is (ListType w) aa, Is (ListType w) bb)
    => Dict (Is (ListType w) (Concat aa bb))
concatIsDict = let
    build :: forall aa'. ListType w aa' -> Dict (Is (ListType w) (Concat aa' bb))
    build NilListType = Dict
    build (ConsListType wa la) =
        case build la of
            Dict ->
                case getRepWitness wa of
                    Dict -> Dict
    in build $ representative @_ @(ListType w) @aa

class ConcatMonoid (f :: [k] -> Type) where
    cmEmpty :: f '[]
    cmAppend :: f a -> f b -> f (Concat a b)
