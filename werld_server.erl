-module(werld_server).
-compile(export_all).

-define(LISTEN_PORT, 9876).
-define(TCP_OPTS, [binary,
                   {packet, raw},
                   {nodelay, true},
                   {reuseaddr, true},
                   {active, once}]).

start() ->
  case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
    {ok, Listen} -> spawn(?MODULE, connect, [Listen]),
      io:format("~p server started~n", [erlang:localtime()]);
    Error ->
      io:format("~p ~p~n", [erlang:localtime(), Error])
  end.

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  inet:setopts(Socket, ?TCP_OPTS),
  spawn(fun() -> connect(Listen) end),
  recv_loop(Socket),
  gen_tcp:close(Socket).

recv_loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Data} ->
      <<Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>> = Data,
      io:format("~p ~p [~w]~n",
                [erlang:localtime(),
                 inet:peername(Socket),
                 {Id, Y, X}]),
      gen_tcp:send(Socket, <<"1">>),
      recv_loop(Socket);
    {tcp_closed, Socket} ->
      io:format("~p disconnect ~w~n", [erlang:localtime(), Socket])
  end.
