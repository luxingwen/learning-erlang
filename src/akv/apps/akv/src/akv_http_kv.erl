-module(akv_http_kv).

-export([init/2]).

init(ReqIn = #{method := <<"GET">>}, State) ->
	{Bucket, Key} = bindings(ReqIn),
	reply(ReqIn, State, akv:get(Bucket, Key));

init(ReqIn = #{method := <<"POST">>}, State) ->
	{Bucket, Key} = bindings(ReqIn),
	{ok, Value, Req} = read_all_body(ReqIn),
	reply(Req, State, akv:put(Bucket, Key, Value));

init(ReqIn = #{method := <<"DELETE">>}, State) ->
	{Bucket, Key} = bindings(ReqIn),
	reply(ReqIn, State, akv:delete(Bucket, Key)).

reply(ReqIn, State, {ok, Replies}) ->
	ReqOut = reply_json(ReqIn, 200, #{replies => [reply_to_data(Reply) || Reply <- Replies]}),
	{ok, ReqOut, State}.

reply_json(ReqIn, Status, Data) ->
	DataEnc = jsx:encode(Data, [{space, 1}, {indent, 2}]),
	cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, DataEnc, ReqIn).


reply_to_data({[Partition, Node], ok}) ->
	#{ok => true, partition => integer_to_binary(Partition), node => Node};

reply_to_data({[Partition, Node], {found, {K, V}}}) ->
	#{ok => true, partition => integer_to_binary(Partition), node => Node, value => V};

reply_to_data({[Partition, Node], {not_found, {Bucket, Key}}}) ->
	#{ok => false, partition => integer_to_binary(Partition), node => Node,
		error => not_found, bucket => Bucket, key => Key
	 }.


read_all_body(ReqIn) ->
	read_all_body(ReqIn, <<>>).

read_all_body(Req0, Acc) ->
	case cowboy_req:read_body(Req0) of
		{ok, Data, Req} ->
			{ok, <<Acc/binary, Data/binary>>, Req};
		{more, Data, Req} ->
			read_all_body(Req, <<Acc/binary, Data/binary>>)
	end.

bindings(ReqIn) ->
	Bucket = cowboy_req:binding(bucket, ReqIn),
	Key = cowboy_req:binding(key, ReqIn),
	{Bucket, Key}.


integer_to_binary(Num) ->
    list_to_binary(integer_to_list(Num)).