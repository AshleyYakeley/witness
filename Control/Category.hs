module Control.Category where
{
	import qualified Prelude;

	-- | ('.') must be associative, and 'id' must be a left and right identity for ('.').
	;
	class Category comp where
	{
		id :: comp a a;
		(.) :: comp b c -> comp a b -> comp a c;
	};

	instance Category (->) where
	{
		id = Prelude.id;
		(.) = (Prelude..);
	};
}
