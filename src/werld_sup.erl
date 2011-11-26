-module(werld_sup).
-compile([export_all]).
-include("../include/client.hrl").
-include("../include/player.hrl").
-include("../include/request_types.hrl").

-define(LISTEN_PORT, 9876).
-define(LISTEN_TCP_OPTIONS, [binary, {packet, raw}, {active, false}]).

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

init() ->
  spawn_link(?MODULE, listener, []),
  register(client_sup, werld_client_sup:start_link()).

listener() ->
  {ok, ListenSocket} = gen_tcp:listen(?LISTEN_PORT, ?LISTEN_TCP_OPTIONS),
  io:format("~p listening on port ~w~n", [erlang:localtime(), ?LISTEN_PORT]),
  spawn(?MODULE, acceptor, [ListenSocket]),
  timer:sleep(infinity).

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  io:format("~p connection ~w~n", [erlang:localtime(), Socket]),
  spawn(?MODULE, acceptor, [ListenSocket]),
  loop(Socket),
  gen_tcp:close(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_PLAYER,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {player, Client},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_PLAYERS>>} ->
      client_sup ! {players, Socket},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_REGISTER,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {register, Client},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_UNREGISTER,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {unregister, Client},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_MESSAGE,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes,
                    Message/binary>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      client_sup ! {message, Client, Message},
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
