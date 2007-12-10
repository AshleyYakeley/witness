module Control.Category where
{
	import qualified Prelude;

	class Category comp where
	{
		id :: comp a a;
		(.) :: comp b c -> comp a b -> comp a c;
	};

	instance Category (->) where
	{
		id = Prelude.id;
		-- (.) = (Prelude..);	haddock chokes on this
		(.) f g a = f (g a);
	};
}
