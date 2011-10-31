-module(werld_client_sup).
-compile([export_all]).
-include("../include/client.hrl").
-include("../include/player.hrl").

start_link() ->
  spawn_link(?MODULE, loop, [[]]).

loop(ClientList) ->
  receive
    {register, Client} ->
      io:format("~p registering ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      notify_clients([Client | werld_client_list:delete(Client, ClientList)]),
      loop([Client | ClientList]);
    {unregister, Client} ->
      io:format("~p unregistering ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      NewClientList = werld_client_list:delete(Client, ClientList),
      notify_clients(NewClientList),
      loop(NewClientList);
    {players, Socket} ->
      io:format("~p players ~w~n", [erlang:localtime(), Socket]),
      PlayerList = werld_client_list:player_list(ClientList),
      Payload = werld_player_list:to_binary(PlayerList),
      PlayerListLength = length(PlayerList),
      Data = <<1:4/native-unit:8, PlayerListLength:4/native-unit:8, Payload/binary>>,
      io:format("~p sending ~B bytes to ~p ~p~n",
                [erlang:localtime(), length(binary_to_list(Data)), Socket, Data]),
      gen_tcp:send(Socket, Data),
      loop(ClientList);
    {player, Client} ->
      io:format("~p player ~w~n", [erlang:localtime(), Client#client.socket]),
      % FIXME: update player state more intelligently.
      NewClientList = [Client | werld_client_list:delete(Client, ClientList)],
      notify_clients(NewClientList),
      loop(NewClientList);
    {message, Client, Message} ->
      io:format("~p message from ~w ~s~n", [erlang:localtime(), Client#client.socket, Message]),
      werld_client_list:multicast_message(Message, Client, ClientList),
      loop(ClientList);
    stop ->
      stop()
  end.

notify_clients(ClientList) ->
  PlayerList = werld_client_list:player_list(ClientList),
  Payload = werld_player_list:to_binary(PlayerList),
  PlayerListLength = length(PlayerList),
  Data = <<1:4/native-unit:8, PlayerListLength:4/native-unit:8, Payload/binary>>,
  io:format("~p sending ~B bytes ~p~n",
            [erlang:localtime(), length(binary_to_list(Data)), Data]),
  [gen_tcp:send(S, Data) || S <- werld_client_list:socket_list(ClientList)].

stop() ->
    ok.
