module Data.Witness
(
    module Data.Proxy,
    module Data.Type.Equality,
    module Data.Witness.Any,
    module Data.Witness.WitnessDict,
    module Data.Witness.WitnessFDict,
    module Data.Witness.Nat,
    module Data.Witness.ListElement,
    module Data.Witness.List,
    module Data.Witness.Representative,
    module Data.Witness
) where
{
    import Data.Proxy;
    import Data.Type.Equality;
    import Data.Witness.Any;
    import Data.Witness.WitnessDict;
    import Data.Witness.WitnessFDict;
    import Data.Witness.Nat;
    import Data.Witness.ListElement;
    import Data.Witness.List;
    import Data.Witness.Representative;

    -- | See whether two represented and witnessed types are the same.
    ;
    matchIs :: forall w a b. (TestEquality w,Is w a,Is w b) => Proxy w -> Maybe (a :~: b);
    matchIs _ = testEquality r r where
    {
        r :: forall t. (Is w t) => w t;
        r = representative;
    };
}
