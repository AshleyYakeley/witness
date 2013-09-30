module Data.Witness.List where
{
    import Data.Witness.Representative;
    import Data.Witness.SimpleWitness;
    import Data.Witness.EqualType;
    import Data.Constraint(Dict(..));
    import Control.Category.Dual;
    import Control.Applicative;
    import Control.Category;
    import Data.Functor.Identity as Import;
    import Prelude hiding (id,(.));

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
        getRepWitness NilListType = MkRepWitness;
        getRepWitness (ConsListType w lw) = case (getRepWitness w,getRepWitness lw) of
        {
            (MkRepWitness,MkRepWitness) -> MkRepWitness;
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

    instance (SimpleWitness w) => SimpleWitness (ListType w) where
    {
        matchWitness NilListType NilListType = Just MkEqualType;
        matchWitness (ConsListType wpa wpb) (ConsListType wqa wqb) = do
        {
            MkEqualType <- matchWitness wpa wqa;
            MkEqualType <- matchWitness wpb wqb;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };

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

    class AppendList (la :: *) (lb :: *) where
    {
        type ListAppend la lb :: *;
        listAppendWitness :: ListType w la -> ListType w lb -> ListType w (ListAppend la lb);
        listJoin :: la -> lb -> ListAppend la lb;
        listSplit :: ListAppend la lb -> (la,lb);
    };

    instance AppendList () lb where
    {
        type ListAppend () lb = lb;
        listAppendWitness NilListType wlb = wlb;
        listJoin () lb = lb;
        listSplit lb = ((),lb);
    };

    instance (AppendList la lb) => AppendList (a,la) lb where
    {
        type ListAppend (a,la) lb = (a,ListAppend la lb);
        listAppendWitness (ConsListType wa wla) wlb = ConsListType wa (listAppendWitness wla wlb);
        listJoin (a,la) lb = (a,listJoin la lb);
        listSplit (a,lab) = case listSplit lab of
        {
            (la,lb) -> ((a,la),lb);
        };
    };

    witnessedListAppend :: ListType w la -> ListType w lb -> Dict (AppendList la lb);
    witnessedListAppend NilListType _ = Dict;
    witnessedListAppend (ConsListType _ wla) wlb = case witnessedListAppend wla wlb of
    {
        Dict -> Dict;
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

    data MapList cc w2 l = forall lr. MkMapList
    {
        listMapWitness :: ListType w2 lr,
        listMap :: cc l lr
    };

    mapList :: (Tensor cc) => MapWitness cc w1 w2 -> ListType w1 l -> MapList cc w2 l;
    mapList _ NilListType = MkMapList
    {
        listMapWitness = NilListType,
        listMap = tensorUnit
    };
    mapList mapwit (ConsListType w rest) = case mapList mapwit rest of
    {
        MkMapList wit listMap' -> mapwit w (\w' vmap -> MkMapList
        {
            listMapWitness = ConsListType w' wit,
            listMap = tensorPair vmap listMap'
        });
    };

    data RemoveFromList w a l = forall lr. MkRemoveFromList
    {
        listRemoveWitness :: ListType w lr,
        listInsert :: a -> lr -> l,
        listRemove :: l -> lr
    };

    removeAllMatching :: (SimpleWitness w) => w a -> ListType w l -> RemoveFromList w a l;
    removeAllMatching _ NilListType = MkRemoveFromList
    {
        listRemoveWitness = NilListType,
        listInsert = \_ -> id,
        listRemove = id
    };
    removeAllMatching wa (ConsListType wb rest) = case removeAllMatching wa rest of
    {
        MkRemoveFromList wit ins rm -> case matchWitness wa wb of
        {
            Just MkEqualType -> MkRemoveFromList
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

    removeAllMatchingMany :: (SimpleWitness wit) => ListType wit lx -> ListType wit l -> RemoveManyFromList wit lx l;
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

    newtype EitherWitness w1 w2 a = MkEitherWitness (Either (w1 a) (w2 a));

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
