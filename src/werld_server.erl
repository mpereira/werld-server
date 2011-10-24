-module(werld_server).
-compile(export_all).
-include("../include/werld_server.hrl").
-include("../include/player.hrl").
-include("../include/client.hrl").

start() ->
  case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTIONS) of
    {ok, Listen} ->
      io:format("~p listening on port ~w~n", [erlang:localtime(), ?LISTEN_PORT]),
      Pid = spawn(fun() -> client_manager([]) end),
      register(client_manager, Pid),
      {ok, spawn(?MODULE, connect, [Listen])};
    Error ->
      io:format("~p ~p~n", [erlang:localtime(), Error])
  end.

client_manager(ClientList) ->
  receive
    {Socket, {all}} ->
      io:format("~p all ~w~n", [erlang:localtime(), Socket]),
      PlayerList = player_list(ClientList),
      Content = case lists:map(fun werld_server:player_to_binary/1, PlayerList) of
                  [] -> <<0>>;
                  L -> list_to_binary(L)
                end,
      PlayerListLength = length(PlayerList),
      gen_tcp:send(Socket, <<PlayerListLength:4/native-unit:8, Content/binary>>),
      client_manager(ClientList);
    {event, Client} ->
      NewClientList = case lists:member(Client#client.socket, client_list_sockets(ClientList)) of
                        true ->
                          [Client | lists:keydelete(Client#client.socket, 2, ClientList)];
                        false ->
                          io:format("~p adding ~s~n", [erlang:localtime(), Client#client.player#player.name]),
                          [Client | ClientList]
                      end,
      gen_tcp:send(Client#client.socket, <<"1">>),
      [send_data(P, ClientList) || P <- lists:keydelete(Client#client.socket, 2, ClientList)],
      client_manager(NewClientList);
    {disconnect, Socket} ->
      client_manager(lists:keydelete(Socket, 2, ClientList))
  end.

client_list_sockets(ClientList) ->
  lists:map(fun(Client) -> Client#client.socket end, ClientList).

send_data(Client, ClientList) ->
  gen_tcp:send(Client#client.socket, lists:map(fun werld_server:player_to_binary/1, player_list(ClientList))).

player_list(ClientList) ->
  lists:map(fun(Client) -> Client#client.player end, ClientList).

player_to_binary(#player{id = Id, name = Name, y = Y, x = X}) ->
  <<Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>.

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:format("~p connect ~w~n", [erlang:localtime(), Socket]),
  inet:setopts(Socket, ?TCP_OPTIONS),
  spawn(fun() -> connect(Listen) end),
  loop(Socket),
  gen_tcp:close(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>} ->
      Player = #player{id = Id, name = Name, y = Y, x = X},
      io:format("~p ~p [~p]~n",
                [erlang:localtime(),
                 inet:peername(Socket),
                 Player]),
      Client = #client{socket = Socket, player = Player},
      client_manager ! {event, Client},
      loop(Socket);
    {tcp, Socket, <<"get_players", _/binary>>} ->
      client_manager ! {Socket, {all}},
      loop(Socket);
    {tcp, Socket, Undefined} ->
      io:format("~p undefined tcp message '~s' from ~w~n",
                [erlang:localtime(), Undefined, Socket]),
      gen_tcp:send(Socket, <<"1">>),
      loop(Socket);
    {tcp_closed, Socket} ->
      client_manager ! {disconnect, Socket},
      io:format("~p disconnect ~w~n", [erlang:localtime(), Socket]);
    Undefined ->
      io:format("~p undefined message '~s' from ~w~n",
                [erlang:localtime(), Undefined, Socket])
  end.

stop(Pid) ->
  Pid ! stop.
