{application, '024-sample-app', [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['aplicacion','db','mercado','subasta','subasta_sup','subastas_handler','usuario','usuario_sup','usuarios_handler','web_server']},
	{registered, []},
	{applications, [kernel,stdlib,qdate,cowboy,uuid]},
	{env, []}
]}.