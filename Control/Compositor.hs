module Control.Compositor where
{
	class Compositor comp where
	{
		identity :: comp a a;
		compose :: comp b c -> comp a b -> comp a c;
	};

	instance Compositor (->) where
	{
		identity = id;
		compose = (.);
	};
}
