{application, 'rest', [
	{description, "rest Api"},
	{vsn, "0.1.0"},
	{modules, ['delivery_handler','rest_app','rest_sup']},
	{registered, [rest_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {rest_app, []}},
	{env, []}
]}.