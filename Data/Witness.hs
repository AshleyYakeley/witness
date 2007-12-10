module Data.Witness
(
	module Data.Witness.Any,
	module Data.Witness.Representative,
	module Data.Witness.Type,
	module Data.Witness.Witness,
	module Data.Witness.SameType,
	module Data.Witness.List,
	module Data.Witness
) where
{
	import Data.Witness.List;
	import Data.Witness.SameType;
	import Data.Witness.Witness;
	import Data.Witness.Type;
	import Data.Witness.Representative;
	import Data.Witness.Any;

	matchIs :: (Witness w,Is w a,Is w a') => Type (w ()) -> Maybe (SameType a a');
	matchIs t = matchWitness (foo t) (foo t) where
	{
		foo :: (Is w a) => Type (w ()) -> w a;
		foo _ = representative;
	};
}
