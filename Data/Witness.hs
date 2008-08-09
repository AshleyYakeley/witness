module Data.Witness
(
	module Data.Witness.Any,
	module Data.Witness.Representative,
	module Data.Witness.Type,
	module Data.Witness.SimpleWitness,
	module Data.Witness.EqualType,
	module Data.Witness.List,
	module Data.Witness
) where
{
	import Data.Witness.List;
	import Data.Witness.EqualType;
	import Data.Witness.SimpleWitness;
	import Data.Witness.Type;
	import Data.Witness.Representative;
	import Data.Witness.Any;

	-- | See whether two represented and witnessed types are the same.
	;
	matchIs :: (SimpleWitness w,Is w a,Is w b) => Type (w ()) -> Maybe (EqualType a b);
	matchIs t = matchWitness (foo t) (foo t) where
	{
		foo :: (Is w a) => Type (w ()) -> w a;
		foo _ = representative;
	};
}
