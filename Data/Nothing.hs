module Data.Nothing where
{
	data Nothing;
	
	never :: Nothing -> a;
	never n = seq n undefined;
}
