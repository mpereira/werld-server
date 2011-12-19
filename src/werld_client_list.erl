%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_client_list.

-module(werld_client_list).
-author('Murilo Pereira <murilo@murilopereira.com>').

-include("../include/client.hrl").
-include("include/response_types.hrl").

-export([player_list/1,
         socket_list/1,
         member/2,
         delete/2,
         find_client_by_socket/2,
         multicast_message/3,
         multicast_player_list/1,
         multicast_player_list_except/2]).

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

find_client_by_socket(Socket, ClientList) ->
  lists:keyfind(Socket, 2, ClientList).

multicast_message(Message, Sender, ClientList) ->
  [werld_client:send_message(Message, Sender, Recipient) ||
   Recipient <- ClientList].

multicast_player_list(ClientList) ->
  multicast_player_list_except([], ClientList).

multicast_player_list_except(Client, ClientList) ->
  PlayerList = werld_client_list:player_list(ClientList),
  Payload = werld_player_list:to_binary(PlayerList),
  PlayerListLength = length(PlayerList),
  Data = <<?WERLD_RESPONSE_TYPE_PLAYERS,
           PlayerListLength:4/native-unit:8,
           Payload/binary>>,
  io:format("~p multicasting ~B bytes ~p~n",
            [erlang:localtime(), length(binary_to_list(Data)), Data]),
  [gen_tcp:send(S, Data) ||
   S <- werld_client_list:socket_list(werld_client_list:delete(Client,
                                                               ClientList))].
