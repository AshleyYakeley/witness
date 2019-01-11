module Data.Witness.HList where

import Control.Applicative
import Control.Category
import Control.Category.Tensor
import Data.Functor.Identity as Import
import Data.Kind
import Data.Type.Equality
import Data.Witness.Either
import Data.Witness.List
import Data.Witness.ListElement
import Prelude hiding ((.), id)

type family HList (w :: [Type]) = r | r -> w where
    HList '[] = ()
    HList (t : tt) = (t, HList tt)

listFill :: ListType w t -> (forall a. w a -> a) -> HList t
listFill NilListType _f = ()
listFill (ConsListType wa wr) f = (f wa, listFill wr f)

listMap :: ListType w t -> (forall a. w a -> a -> a) -> HList t -> HList t
listMap NilListType _f () = ()
listMap (ConsListType wa wr) f (a, rest) = (f wa a, listMap wr f rest)

listLift2 :: ListType w t -> (forall a. w a -> a -> a -> a) -> HList t -> HList t -> HList t
listLift2 NilListType _f () () = ()
listLift2 (ConsListType wa wr) f (a, resta) (b, restb) = (f wa a b, listLift2 wr f resta restb)

listTypeToList :: (forall a. w a -> r) -> ListType w t -> [r]
listTypeToList _wr NilListType = []
listTypeToList wr (ConsListType wa rest) = (wr wa) : (listTypeToList wr rest)

listTypeMap :: (forall a. w1 a -> w2 a) -> ListType w1 t -> ListType w2 t
listTypeMap _ww NilListType = NilListType
listTypeMap ww (ConsListType wa rest) = ConsListType (ww wa) (listTypeMap ww rest)

listIdentity :: ListType Identity lt -> HList lt
listIdentity NilListType = ()
listIdentity (ConsListType (Identity a) rest) = (a, listIdentity rest)

listSequence :: (Applicative f) => ListType f lt -> f (HList lt)
listSequence NilListType = pure ()
listSequence (ConsListType fa rest) = liftA2 (,) fa (listSequence rest)

getListElement :: ListElementType list t -> HList list -> t
getListElement FirstElementType (a, _) = a
getListElement (RestElementType lw) (_, r) = getListElement lw r

putListElement :: ListElementType list t -> t -> HList list -> HList list
putListElement FirstElementType t (_, r) = (t, r)
putListElement (RestElementType lw) t (a, r) = (a, putListElement lw t r)

modifyListElement :: ListElementType list t -> (t -> t) -> HList list -> HList list
modifyListElement n aa t = putListElement n (aa (getListElement n t)) t

data AppendList w la lb = forall lr. MkAppendList
    { listAppendWitness :: ListType w lr
    , listAppend :: HList la -> HList lb -> HList lr
    , listSplit :: HList lr -> (HList la, HList lb)
    }

appendList :: ListType w la -> ListType w lb -> AppendList w la lb
appendList NilListType wlb =
    MkAppendList {listAppendWitness = wlb, listAppend = \() lb -> lb, listSplit = \lb -> ((), lb)}
appendList (ConsListType wa wla) wlb =
    case appendList wla wlb of
        MkAppendList wit join split ->
            MkAppendList
                { listAppendWitness = ConsListType wa wit
                , listAppend = \(a, la) lb -> (a, join la lb)
                , listSplit =
                      \(a, lab) ->
                          case split lab of
                              (la, lb) -> ((a, la), lb)
                }

data AddItemList w a l = forall lr. MkAddItemList
    { listAddItemWitness :: ListType w lr
    , listAddItem :: a -> HList l -> HList lr
    , listSplitItem :: HList lr -> (a, HList l)
    }

addListItem :: w a -> ListType w l -> AddItemList w a l
addListItem wa wl = MkAddItemList {listAddItemWitness = ConsListType wa wl, listAddItem = (,), listSplitItem = id}

data MergeItemList w a l = forall lr. MkMergeItemList
    { listMergeItemWitness :: ListType w lr
    , listMergeItem :: (Maybe a -> a) -> HList l -> HList lr
    , listUnmergeItem :: HList lr -> (a, HList l)
    }

mergeListItem :: (TestEquality w) => ListType w l -> w a -> MergeItemList w a l
mergeListItem NilListType wa =
    MkMergeItemList
        { listMergeItemWitness = ConsListType wa NilListType
        , listMergeItem = \maa () -> (maa Nothing, ())
        , listUnmergeItem = id
        }
mergeListItem wl@(ConsListType wa' _) wa
    | Just Refl <- testEquality wa wa' =
        MkMergeItemList
            { listMergeItemWitness = wl
            , listMergeItem = \maa (a, l) -> (maa (Just a), l)
            , listUnmergeItem = \(a, l) -> (a, (a, l))
            }
mergeListItem (ConsListType wa' wl) wa =
    case mergeListItem wl wa of
        MkMergeItemList wit merge unmerge ->
            MkMergeItemList
                { listMergeItemWitness = ConsListType wa' wit
                , listMergeItem = \maa (a', l) -> (a', merge maa l)
                , listUnmergeItem =
                      \(a', l') ->
                          case unmerge l' of
                              (a, l) -> (a, (a', l))
                }

data MergeList w la lb = forall lr. MkMergeList
    { listMergeWitness :: ListType w lr
    , listMerge :: (forall t. w t -> t -> t -> t) -> HList la -> HList lb -> HList lr
    , listUnmerge :: HList lr -> (HList la, HList lb)
    }

mergeList :: (TestEquality w) => ListType w la -> ListType w lb -> MergeList w la lb
mergeList wla NilListType =
    MkMergeList {listMergeWitness = wla, listMerge = \_ la () -> la, listUnmerge = \la -> (la, ())}
mergeList wla (ConsListType wb wlb) =
    case mergeListItem wla wb of
        MkMergeItemList wla' mergeItem unmergeItem ->
            case mergeList wla' wlb of
                MkMergeList wlr merge unmerge ->
                    MkMergeList
                        { listMergeWitness = wlr
                        , listMerge =
                              \f la (b, lb) ->
                                  merge
                                      f
                                      (mergeItem
                                           (\mb' ->
                                                case mb' of
                                                    Just b' -> f wb b' b
                                                    Nothing -> b)
                                           la)
                                      lb
                        , listUnmerge =
                              \lr ->
                                  case unmerge lr of
                                      (la', lb) ->
                                          case unmergeItem la' of
                                              (b, la) -> (la, (b, lb))
                        }

type MapWitness cc w1 w2 = forall r v1. w1 v1 -> (forall v2. w2 v2 -> (cc v1 v2) -> r) -> r

sameMapWitness :: (forall v. w v -> cc v v) -> MapWitness cc w w
sameMapWitness wc w wcr = wcr w (wc w)

data MapList cc w2 l = forall lr. MkMapList
    { listMapWitness :: ListType w2 lr
    , listMapW :: cc (HList l) (HList lr)
    }

mapList :: (Tensor cc) => MapWitness cc w1 w2 -> ListType w1 l -> MapList cc w2 l
mapList _ NilListType = MkMapList {listMapWitness = NilListType, listMapW = tensorUnit}
mapList mapwit (ConsListType w rest) =
    case mapList mapwit rest of
        MkMapList wit listMapW' ->
            mapwit
                w
                (\w' vmap -> MkMapList {listMapWitness = ConsListType w' wit, listMapW = tensorPair vmap listMapW'})

data RemoveFromList w a l = forall lr. MkRemoveFromList
    { listRemoveWitness :: ListType w lr
    , listInsert :: a -> HList lr -> HList l
    , listRemove :: HList l -> HList lr
    }

removeAllMatching :: (TestEquality w) => w a -> ListType w l -> RemoveFromList w a l
removeAllMatching _ NilListType =
    MkRemoveFromList {listRemoveWitness = NilListType, listInsert = \_ -> id, listRemove = id}
removeAllMatching wa (ConsListType wb rest) =
    case removeAllMatching wa rest of
        MkRemoveFromList wit ins rm ->
            case testEquality wa wb of
                Just Refl ->
                    MkRemoveFromList
                        {listRemoveWitness = wit, listInsert = \a l2 -> (a, ins a l2), listRemove = \(_, l1) -> rm l1}
                Nothing ->
                    MkRemoveFromList
                        { listRemoveWitness = ConsListType wb wit
                        , listInsert = \a (b, l2) -> (b, ins a l2)
                        , listRemove = \(b, l1) -> (b, rm l1)
                        }

data RemoveManyFromList wit lx l = forall lr. MkRemoveManyFromList
    { listRemoveManyWitness :: ListType wit lr
    , listInsertMany :: HList lx -> HList lr -> HList l
    , listRemoveMany :: HList l -> HList lr
    }

removeAllMatchingMany :: (TestEquality wit) => ListType wit lx -> ListType wit l -> RemoveManyFromList wit lx l
removeAllMatchingMany NilListType wl =
    MkRemoveManyFromList {listRemoveManyWitness = wl, listInsertMany = \_ lr -> lr, listRemoveMany = \l -> l}
removeAllMatchingMany (ConsListType wa wlx) wl =
    case removeAllMatching wa wl of
        MkRemoveFromList wl' ins rm ->
            case removeAllMatchingMany wlx wl' of
                MkRemoveManyFromList wl'' insM remM ->
                    MkRemoveManyFromList
                        { listRemoveManyWitness = wl''
                        , listInsertMany = \(a, lx) lr -> ins a (insM lx lr)
                        , listRemoveMany = remM . rm
                        }

data PartitionList wit1 wit2 l = forall l1 l2. MkPartitionList
    { listPartitionWitness1 :: ListType wit1 l1
    , listPartitionWitness2 :: ListType wit2 l2
    , listFromPartition :: HList l1 -> HList l2 -> HList l
    , listToPartition1 :: HList l -> HList l1
    , listToPartition2 :: HList l -> HList l2
    }

partitionList :: ListType (EitherType w1 w2) l -> PartitionList w1 w2 l
partitionList NilListType =
    MkPartitionList
        { listPartitionWitness1 = NilListType
        , listPartitionWitness2 = NilListType
        , listFromPartition = \() () -> ()
        , listToPartition1 = \() -> ()
        , listToPartition2 = \() -> ()
        }
partitionList (ConsListType (LeftType w1a) rest) =
    case partitionList rest of
        MkPartitionList pw1 pw2 fp tp1 tp2 ->
            MkPartitionList
                { listPartitionWitness1 = ConsListType w1a pw1
                , listPartitionWitness2 = pw2
                , listFromPartition = \(a, l1) l2 -> (a, fp l1 l2)
                , listToPartition1 = \(a, l) -> (a, tp1 l)
                , listToPartition2 = \(_, l) -> tp2 l
                }
partitionList (ConsListType (RightType w2a) rest) =
    case partitionList rest of
        MkPartitionList pw1 pw2 fp tp1 tp2 ->
            MkPartitionList
                { listPartitionWitness1 = pw1
                , listPartitionWitness2 = ConsListType w2a pw2
                , listFromPartition = \l1 (a, l2) -> (a, fp l1 l2)
                , listToPartition1 = \(_, l) -> tp1 l
                , listToPartition2 = \(a, l) -> (a, tp2 l)
                }
