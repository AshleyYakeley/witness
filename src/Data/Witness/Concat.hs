module Data.Witness.Concat where

import Data.Constraint (Dict(..))
import Data.Kind
import Data.Type.Equality
import Data.Witness.List
import Data.Witness.Representative
import Prelude hiding ((.), id)

type Concat :: forall k. [k] -> [k] -> [k]
type family Concat a b where
    Concat '[] bb = bb
    Concat (a ': aa) bb = a ': (Concat aa bb)

concatEmptyRefl :: ListType w a -> Concat a '[] :~: a
concatEmptyRefl NilListType = Refl
concatEmptyRefl (ConsListType _ la) =
    case concatEmptyRefl la of
        Refl -> Refl

concatIsDict ::
       forall k (w :: k -> Type) (aa :: [k]) (bb :: [k]). (Representative w, Is (ListType w) aa, Is (ListType w) bb)
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

concatListType ::
       forall k (w :: k -> Type) (a :: [k]) (b :: [k]). ListType w a -> ListType w b -> ListType w (Concat a b)
concatListType NilListType lb = lb
concatListType (ConsListType wa la) lb = ConsListType wa $ concatListType la lb
