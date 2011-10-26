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
      NewClientList =
        case werld_client_list:member(Client, ClientList) of
          true ->
            % FIXME: update player sendind event more inteligently.
            [Client | werld_client_list:delete(Client, ClientList)];
          false ->
            io:format("~p adding ~s~n",
                      [erlang:localtime(), Client#client.player#player.name]),
            [Client | ClientList]
        end,
      gen_tcp:send(Client#client.socket, <<1>>),
      loop(NewClientList);
    {disconnect, Socket} ->
      io:format("~p disconnect ~w~n", [erlang:localtime(), Socket]),
      loop(lists:keydelete(Socket, 2, ClientList));
    stop ->
      stop()
  end.

stop() ->
    ok.
