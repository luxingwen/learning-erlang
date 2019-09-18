-module(erlcount_lib).

-export([find_erl/1]).
-export([regex_count/2]).

-include_lib("kernel/include/file.hrl").

find_erl(Directory) ->
	find_erl(Directory, queue:new()).

find_erl(Name, Queue) ->
	{ok, F = #file_info{}} = file:read_file_info(Name),
	case F#file_info.type of
		directory ->
			handle_directory(Name, Queue);
		regular ->
			handle_regular_file(Name, Queue);
		_Other ->
			dequeue_and_run(Queue)
	end.

%% 打开目录， 把其中的文件放入队列
handle_directory(Dir, Queue) ->
	case file:list_dir(Dir) of
		{ok, []} ->
			dequeue_and_run(Queue);
		{ok, Files} ->
			dequeue_and_run(enqueue_many(Dir, Files, Queue))
	end.


%% 从队列中取出一个元素 ，以它为起点运行
dequeue_and_run(Queue) ->
	case queue:out(Queue) of
		{empty, _} -> done;
		{{value, File}, NewQueue} ->
			find_erl(File, NewQueue)
	end.

%% 把一批文件加入队列
enqueue_many(Path, Files, Queue) ->
	F = fun(File, Q) ->
			queue:in(filename:join(Path, File), Q)
		end,
	lists:foldl(F, Queue, Files).


%% 检查文件是否以 erl结尾
handle_regular_file(Name, Queue) ->
	case filename:extension(Name) of
		".erl" ->
			{continue, Name, fun() -> dequeue_and_run(Queue) end};
		_NonErl ->
			dequeue_and_run(Queue)
	end.


regex_count(Re, Str) ->
	case re:run(Str, Re, [global]) of
		nomatch -> 0;
		{match, List} -> length(List)
	end.