{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Data.Witness.List where
{
    import Prelude hiding (id,(.));
    import Data.Witness.Representative;
    import Data.Type.Equality;
    import Data.Semigroupoid.Dual;
    import Data.Constraint(Dict(..));
    import Control.Applicative;
    import Control.Category;
    import Data.Functor.Identity as Import;

    -- | a witness type for HList-style lists.
    -- The @w@ parameter is the witness type of the elements.
    ;
    data ListType (w :: * -> *) (lt :: *) where
    {
        NilListType :: ListType w ();
        ConsListType :: w a -> ListType w lt -> ListType w (a,lt);
    };

    instance Eq1 w => Eq1 (ListType w) where
    {
        equals1 NilListType NilListType = True;
        equals1 (ConsListType pe pl) (ConsListType qe ql) = (equals1 pe qe) && (equals1 pl ql);
        equals1 _ _ = False;
    };

    instance Eq1 w => Eq (ListType w a) where
    {
        (==) = equals1;
    };

    instance (Representative w) => Representative (ListType w) where
    {
        getRepWitness NilListType = Dict;
        getRepWitness (ConsListType w lw) = case (getRepWitness w,getRepWitness lw) of
        {
            (Dict,Dict) -> Dict;
        };
    };

    instance (Representative w) => Is (ListType w) () where
    {
        representative = NilListType;
    };

    instance (Is w a,Is (ListType w) lt) => Is (ListType w) (a,lt) where
    {
        representative = ConsListType representative representative;
    };

    instance (TestEquality w) => TestEquality (ListType w) where
    {
        testEquality NilListType NilListType = Just Refl;
        testEquality (ConsListType wpa wpb) (ConsListType wqa wqb) = do
        {
            Refl <- testEquality wpa wqa;
            Refl <- testEquality wpb wqb;
            return Refl;
        };
        testEquality _ _ = Nothing;
    };

    listFill :: ListType w t -> (forall a. w a -> a) -> t;
    listFill NilListType _f = ();
    listFill (ConsListType wa wr) f = (f wa,listFill wr f);

    listMap :: ListType w t -> (forall a. w a -> a -> a) -> t -> t;
    listMap NilListType _f () = ();
    listMap (ConsListType wa wr) f (a,rest) = (f wa a,listMap wr f rest);

    listLift2 :: ListType w t -> (forall a. w a -> a -> a -> a) -> t -> t -> t;
    listLift2 NilListType _f () () = ();
    listLift2 (ConsListType wa wr) f (a,resta) (b,restb) = (f wa a b,listLift2 wr f resta restb);

    listTypeToList :: (forall a. w a -> r) -> ListType w t -> [r];
    listTypeToList _wr NilListType = [];
    listTypeToList wr (ConsListType wa rest) = (wr wa):(listTypeToList wr rest);

    listTypeMap :: (forall a. w1 a -> w2 a) -> ListType w1 t -> ListType w2 t;
    listTypeMap _ww NilListType = NilListType;
    listTypeMap ww (ConsListType wa rest) = ConsListType (ww wa) (listTypeMap ww rest);

    listIdentity :: ListType Identity lt -> lt;
    listIdentity NilListType = ();
    listIdentity (ConsListType (Identity a) rest) = (a,listIdentity rest);

    listSequence ::  (Applicative f) => ListType f lt -> f lt;
    listSequence NilListType = pure ();
    listSequence (ConsListType fa rest) = liftA2 (,) fa (listSequence rest);

    data AppendList w la lb = forall lr. MkAppendList
    {
        listAppendWitness :: ListType w lr,
        listAppend :: la -> lb -> lr,
        listSplit :: lr -> (la,lb)
    };

    appendList :: ListType w la -> ListType w lb -> AppendList w la lb;
    appendList NilListType wlb = MkAppendList
    {
        listAppendWitness = wlb,
        listAppend = \() lb -> lb,
        listSplit = \lb -> ((),lb)
    };
    appendList (ConsListType wa wla) wlb = case appendList wla wlb of
    {
        MkAppendList wit join split -> MkAppendList
        {
            listAppendWitness = ConsListType wa wit,
            listAppend = \(a,la) lb -> (a,join la lb),
            listSplit = \(a,lab) -> case split lab of
            {
                (la,lb) -> ((a,la),lb);
            }
        };
    };

    data AddItemList w a l = forall lr. MkAddItemList
    {
        listAddItemWitness :: ListType w lr,
        listAddItem :: a -> l -> lr,
        listSplitItem :: lr -> (a,l)
    };

    addListItem :: w a -> ListType w l -> AddItemList w a l;
    addListItem wa wl = MkAddItemList
    {
        listAddItemWitness = ConsListType wa wl,
        listAddItem = (,),
        listSplitItem = id
    };

    data MergeItemList w a l = forall lr. MkMergeItemList
    {
        listMergeItemWitness :: ListType w lr,
        listMergeItem :: (Maybe a -> a) -> l -> lr,
        listUnmergeItem :: lr -> (a,l)
    };

    mergeListItem :: (TestEquality w) => ListType w l -> w a -> MergeItemList w a l;
    mergeListItem NilListType wa = MkMergeItemList
    {
        listMergeItemWitness = ConsListType wa NilListType,
        listMergeItem = \maa () -> (maa Nothing,()),
        listUnmergeItem = id
    };
    mergeListItem wl@(ConsListType wa' _) wa | Just Refl <- testEquality wa wa' = MkMergeItemList
    {
        listMergeItemWitness = wl,
        listMergeItem = \maa (a,l) -> (maa (Just a),l),
        listUnmergeItem = \(a,l) -> (a,(a,l))
    };
    mergeListItem (ConsListType wa' wl) wa = case mergeListItem wl wa of
    {
        MkMergeItemList wit merge unmerge -> MkMergeItemList
        {
            listMergeItemWitness = ConsListType wa' wit,
            listMergeItem = \maa (a',l) -> (a',merge maa l),
            listUnmergeItem = \(a',l') -> case unmerge l' of
            {
                (a,l) -> (a,(a',l));
            }
        };
    };

    data MergeList w la lb = forall lr. MkMergeList
    {
        listMergeWitness :: ListType w lr,
        listMerge :: (forall t. w t -> t -> t -> t) -> la -> lb -> lr,
        listUnmerge :: lr -> (la,lb)
    };

    mergeList :: (TestEquality w) => ListType w la -> ListType w lb -> MergeList w la lb;
    mergeList wla NilListType = MkMergeList
    {
        listMergeWitness = wla,
        listMerge = \_ la () -> la,
        listUnmerge = \la -> (la,())
    };
    mergeList wla (ConsListType wb wlb) = case mergeListItem wla wb of
    {
        MkMergeItemList wla' mergeItem unmergeItem -> case mergeList wla' wlb of
        {
            MkMergeList wlr merge unmerge -> MkMergeList
            {
                listMergeWitness = wlr,
                listMerge = \f la (b,lb) -> merge f (mergeItem (\mb' -> case mb' of
                {
                    Just b' -> f wb b' b;
                    Nothing -> b;
                }) la) lb,
                listUnmerge = \lr -> case unmerge lr of
                {
                    (la',lb) -> case unmergeItem la' of
                    {
                        (b,la) -> (la,(b,lb));
                    };
                }
            };
        };
    };

    -- could use data-lens:Control.Category.Product(Tensor)
    class Tensor cc where
    {
        tensorUnit :: cc () ();
        tensorPair :: cc a1 b1 -> cc a2 b2 -> cc (a1,a2) (b1,b2);
    };

    instance Tensor (->) where
    {
        tensorUnit = id;
        tensorPair ab1 ab2 (a1,a2) = (ab1 a1,ab2 a2);
    };

    instance (Tensor cc) => Tensor (Dual cc) where
    {
        tensorUnit = Dual tensorUnit;
        tensorPair (Dual ab1) (Dual ab2) = Dual (tensorPair ab1 ab2);
    };

    type MapWitness cc w1 w2 = forall r v1. w1 v1 -> (forall v2. w2 v2 -> (cc v1 v2) -> r) -> r;

    sameMapWitness :: (forall v. w v -> cc v v) -> MapWitness cc w w;
    sameMapWitness wc w wcr = wcr w (wc w);

    data MapList cc w2 l = forall lr. MkMapList
    {
        listMapWitness :: ListType w2 lr,
        listMapW :: cc l lr
    };

    mapList :: (Tensor cc) => MapWitness cc w1 w2 -> ListType w1 l -> MapList cc w2 l;
    mapList _ NilListType = MkMapList
    {
        listMapWitness = NilListType,
        listMapW = tensorUnit
    };
    mapList mapwit (ConsListType w rest) = case mapList mapwit rest of
    {
        MkMapList wit listMapW' -> mapwit w (\w' vmap -> MkMapList
        {
            listMapWitness = ConsListType w' wit,
            listMapW = tensorPair vmap listMapW'
        });
    };

    data RemoveFromList w a l = forall lr. MkRemoveFromList
    {
        listRemoveWitness :: ListType w lr,
        listInsert :: a -> lr -> l,
        listRemove :: l -> lr
    };

    removeAllMatching :: (TestEquality w) => w a -> ListType w l -> RemoveFromList w a l;
    removeAllMatching _ NilListType = MkRemoveFromList
    {
        listRemoveWitness = NilListType,
        listInsert = \_ -> id,
        listRemove = id
    };
    removeAllMatching wa (ConsListType wb rest) = case removeAllMatching wa rest of
    {
        MkRemoveFromList wit ins rm -> case testEquality wa wb of
        {
            Just Refl -> MkRemoveFromList
            {
                listRemoveWitness = wit,
                listInsert = \a l2 -> (a,ins a l2),
                listRemove = \(_,l1) -> rm l1
            };
            Nothing -> MkRemoveFromList
            {
                listRemoveWitness = ConsListType wb wit,
                listInsert = \a (b,l2) -> (b,ins a l2),
                listRemove = \(b,l1) -> (b,rm l1)
            };
        };
    };

    data RemoveManyFromList wit lx l = forall lr. MkRemoveManyFromList
    {
        listRemoveManyWitness :: ListType wit lr,
        listInsertMany :: lx -> lr -> l,
        listRemoveMany :: l -> lr
    };

    removeAllMatchingMany :: (TestEquality wit) => ListType wit lx -> ListType wit l -> RemoveManyFromList wit lx l;
    removeAllMatchingMany NilListType wl = MkRemoveManyFromList
    {
        listRemoveManyWitness = wl,
        listInsertMany = \_ lr -> lr,
        listRemoveMany = \l -> l
    };
    removeAllMatchingMany (ConsListType wa wlx) wl = case removeAllMatching wa wl of
    {
        MkRemoveFromList wl' ins rm -> case removeAllMatchingMany wlx wl' of
        {
            MkRemoveManyFromList wl'' insM remM -> MkRemoveManyFromList
            {
                listRemoveManyWitness = wl'',
                listInsertMany = \(a,lx) lr -> ins a (insM lx lr),
                listRemoveMany = remM . rm
            };
        };
    };

    newtype EitherWitness (w1 :: k -> *) (w2 :: k -> *) (a :: k) = MkEitherWitness (Either (w1 a) (w2 a));

    instance (TestEquality w1,TestEquality w2) => TestEquality (EitherWitness w1 w2) where
    {
        testEquality (MkEitherWitness (Left wa)) (MkEitherWitness (Left wb)) = testEquality wa wb;
        testEquality (MkEitherWitness (Right wa)) (MkEitherWitness (Right wb)) = testEquality wa wb;
        testEquality _ _ = Nothing;
    };

    data PartitionList wit1 wit2 l = forall l1 l2. MkPartitionList
    {
        listPartitionWitness1 :: ListType wit1 l1,
        listPartitionWitness2 :: ListType wit2 l2,
        listFromPartition :: l1 -> l2 -> l,
        listToPartition1 :: l -> l1,
        listToPartition2 :: l -> l2
    };

    partitionList :: ListType (EitherWitness w1 w2) l -> PartitionList w1 w2 l;
    partitionList NilListType = MkPartitionList
    {
        listPartitionWitness1 = NilListType,
        listPartitionWitness2 = NilListType,
        listFromPartition = \() () -> (),
        listToPartition1 = \() -> (),
        listToPartition2 = \() -> ()
    };
    partitionList (ConsListType (MkEitherWitness (Left w1a)) rest) = case partitionList rest of
    {
        MkPartitionList pw1 pw2 fp tp1 tp2 -> MkPartitionList
        {
            listPartitionWitness1 = ConsListType w1a pw1,
            listPartitionWitness2 = pw2,
            listFromPartition = \(a,l1) l2 -> (a,fp l1 l2),
            listToPartition1 = \(a,l) -> (a,tp1 l),
            listToPartition2 = \(_,l) -> tp2 l
        };
    };
    partitionList (ConsListType (MkEitherWitness (Right w2a)) rest) = case partitionList rest of
    {
        MkPartitionList pw1 pw2 fp tp1 tp2 -> MkPartitionList
        {
            listPartitionWitness1 = pw1,
            listPartitionWitness2 = ConsListType w2a pw2,
            listFromPartition = \l1 (a,l2) -> (a,fp l1 l2),
            listToPartition1 = \(_,l) -> tp1 l,
            listToPartition2 = \(a,l) -> (a,tp2 l)
        };
    };
}
