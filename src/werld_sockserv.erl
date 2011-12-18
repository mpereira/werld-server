-module(werld_sockserv).

-include("../include/client.hrl").
-include("../include/player.hrl").
-include("../include/request_types.hrl").

-export([start_link/0]).

-export([init/0, listener/0, acceptor/1, loop/1, stop/0]).

-define(LISTEN_PORT, 9876).
-define(LISTEN_TCP_OPTIONS, [binary, {packet, raw}, {reuseaddr, true}, {active, false}]).

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

init() ->
  spawn_link(?MODULE, listener, []),
  register(werld_evserv, werld_evserv:start_link()).

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
      werld_evserv ! {player, Client},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_PLAYERS>>} ->
      werld_evserv ! {players, Socket},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_REGISTER,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      werld_evserv ! {register, Client},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_MESSAGE,
                    Id:4/bytes,
                    Name:20/bytes,
                    Y:4/bytes,
                    X:4/bytes,
                    Message/binary>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      Client = #client{socket = Socket, player = Player},
      werld_evserv ! {message, Client, Message},
      loop(Socket);
    {tcp, Socket, <<?WERLD_REQUEST_TYPE_MAP, Map:8/unsigned>>} ->
      werld_evserv ! {map, Socket, Map},
      loop(Socket);
    {tcp, Socket, Undefined} ->
      io:format("~p undefined tcp message '~s' from ~w~n",
                [erlang:localtime(), Undefined, Socket]),
      gen_tcp:send(Socket, <<-1>>),
      loop(Socket);
    {tcp_closed, Socket} ->
      werld_evserv ! {disconnect, Socket};
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
