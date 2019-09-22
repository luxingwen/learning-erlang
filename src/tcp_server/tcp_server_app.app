{application, tcp_server_app,
	[{vsn, "1.0.0"},
	 {description, "A simple tcp_server demo"},
	 {modules, [tcp_server_app, tcp_server, tcp_server_sup]},
	 {applications, [stdlib, kernel, sasl]},
	 {registered, [tcp_server_app]},
	 {mod, {tcp_server_app, []}},
	 {env, [
	 	{port, 8090}
	 ]}
	]
}.