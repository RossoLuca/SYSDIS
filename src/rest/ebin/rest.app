{application, 'rest', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['active_drone_handler','db_initializer','delivery_handler','rest_app','rest_sup','spawn_recovery_handler']},
	{registered, [rest_sup]},
	{applications, [kernel,stdlib,cowboy,jiffy]},
	{mod, {rest_app, []}},
	{env, []}
]}.