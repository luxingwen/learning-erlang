{application, m8ball,
	[{vsn, "1.0.0"},
	 {description, "Answer vital questions"},
	 {modules, [m8ball, m8ball_sup, m8ball_server]},
	 {applications, [stdlib, kernel, crypto]},
	 {registered, [m8ball, m8ball_sup, m8ball_server]},
	 {mod, {m8ball, []}},
	 {env, [
	 	{answers, {<<"Yes">>, <<"No">>, <<"Doubtful">>,
	 			  <<"I don't like you tone">>, <<"of course">>,
	 			  <<"Of course not">>, <<"*backs away showly and runs away*">>}
	 	}
	 ]}
	]
}.