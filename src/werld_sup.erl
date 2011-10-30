-module(werld_sup).
-compile([export_all]).
-include("../include/werld.hrl").
-include("../include/client.hrl").
-include("../include/player.hrl").

start_link() ->
  spawn_link(?MODULE, init, []).

init() ->
  Pid = spawn_link(fun() ->
    {ok, Listen} = gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTIONS),
    io:format("~p listening on port ~w~n", [erlang:localtime(), ?LISTEN_PORT]),
    register(client_sup, werld_client_sup:start_link()),
    spawn_link(?MODULE, connect, [Listen]),
    timer:sleep(infinity)
  end),
  {ok, Pid}.

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:format("~p connect ~w~n", [erlang:localtime(), Socket]),
  inet:setopts(Socket, ?TCP_OPTIONS),
  spawn(?MODULE, connect, [Listen]),
  loop(Socket),
  gen_tcp:close(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"player", Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {player, Client},
      loop(Socket);
    {tcp, Socket, <<"register", Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {register, Client},
      loop(Socket);
    {tcp, Socket, <<"unregister", Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {unregister, Client},
      loop(Socket);
    {tcp, Socket, <<"players", _/binary>>} ->
      client_sup ! {players, Socket},
      loop(Socket);
    {tcp, Socket, Undefined} ->
      io:format("~p undefined tcp message '~s' from ~w~n",
                [erlang:localtime(), Undefined, Socket]),
      gen_tcp:send(Socket, <<-1>>),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("~p disconnect ~w~n", [erlang:localtime(), Socket]);
    stop ->
      stop();
    Undefined ->
      io:format("~p undefined message '~s' from ~w~n",
                [erlang:localtime(), Undefined, Socket]),
      gen_tcp:send(Socket, <<-1>>),
      loop(Socket)
  end.

stop() ->
  ok.
