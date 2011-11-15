-module(werld_client_list).
-compile([export_all]).
-include("../include/client.hrl").
-include("include/response_types.hrl").

player_list(ClientList) ->
  lists:map(fun(Client) -> Client#client.player end, ClientList).

socket_list(ClientList) ->
  lists:map(fun(Client) -> Client#client.socket end, ClientList).

member(Client, ClientList) ->
  lists:member(Client#client.socket, socket_list(ClientList)).

delete([], ClientList) ->
  ClientList;
delete(Client, ClientList) ->
  lists:keydelete(Client#client.socket, 2, ClientList).

multicast_message(Message, Sender, ClientList) ->
  [werld_client:send_message(Message, Sender, Recipient) ||
   Recipient <- ClientList].

multicast_player_list(ClientList) ->
  multicast_player_list_except([], ClientList).

multicast_player_list_except(Client, ClientList) ->
  PlayerList = werld_client_list:player_list(ClientList),
  Payload = werld_player_list:to_binary(PlayerList),
  PlayerListLength = length(PlayerList),
  Data = <<?WERLD_RESPONSE_TYPE_PLAYERS:4/native-unit:8,
           PlayerListLength:4/native-unit:8,
           Payload/binary>>,
  io:format("~p multicasting ~B bytes ~p~n",
            [erlang:localtime(), length(binary_to_list(Data)), Data]),
  [gen_tcp:send(S, Data) ||
   S <- werld_client_list:socket_list(werld_client_list:delete(Client,
                                                               ClientList))].
