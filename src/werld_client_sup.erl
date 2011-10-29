-module(werld_client_sup).
-compile([export_all]).
-include("../include/client.hrl").
-include("../include/player.hrl").

start_link() ->
  spawn_link(?MODULE, loop, [[]]).

loop(ClientList) ->
  receive
    {Socket, {all}} ->
      io:format("~p all ~w~n", [erlang:localtime(), Socket]),
      PlayerList = werld_client_list:player_list(ClientList),
      Payload = werld_player_list:to_binary(PlayerList),
      PlayerListLength = length(PlayerList),
      Data = <<PlayerListLength:4/native-unit:8, Payload/binary>>,
      io:format("~p sending ~B bytes ~p~n",
                [erlang:localtime(), length(binary_to_list(Data)), Data]),
      gen_tcp:send(Socket, Data),
      loop(ClientList);
    {event, Client} ->
      io:format("~p event ~w~n", [erlang:localtime(), Client#client.socket]),

      PlayerList = werld_client_list:player_list(ClientList),
      Payload = werld_player_list:to_binary(PlayerList),
      PlayerListLength = length(PlayerList),
      Data = <<PlayerListLength:4/native-unit:8, Payload/binary>>,
      io:format("~p sending ~B bytes ~p~n",
                [erlang:localtime(), length(binary_to_list(Data)), Data]),
      [gen_tcp:send(S, Data) || S <- werld_client_list:socket_list(ClientList)],
      % FIXME: update player state more intelligently.
      loop([Client | werld_client_list:delete(Client, ClientList)]);
    {register, Client} ->
      io:format("~p registering ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      loop([Client | ClientList]);
    {disconnect, Socket} ->
      io:format("~p disconnect ~w~n", [erlang:localtime(), Socket]),
      loop(lists:keydelete(Socket, 2, ClientList));
    stop ->
      stop()
  end.

stop() ->
    ok.
