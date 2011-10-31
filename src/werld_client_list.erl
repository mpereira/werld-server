-module(werld_client_list).
-compile([export_all]).
-include("../include/client.hrl").

player_list(ClientList) ->
  lists:map(fun(Client) -> Client#client.player end, ClientList).

socket_list(ClientList) ->
  lists:map(fun(Client) -> Client#client.socket end, ClientList).

member(Client, ClientList) ->
  lists:member(Client#client.socket, socket_list(ClientList)).

delete(Client, ClientList) ->
  lists:keydelete(Client#client.socket, 2, ClientList).

multicast_message(Message, Sender, ClientList) ->
  [werld_client:send_message(Message, Sender, Recipient) || Recipient <- ClientList].
