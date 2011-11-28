-module(werld_client).
-compile([export_all]).
-include("include/client.hrl").
-include("include/maps.hrl").
-include("include/player.hrl").
-include("include/response_types.hrl").

send_map(Map, Client) ->
  Payload = [<<?WERLD_RESPONSE_TYPE_MAP>>,
             <<?WERLD_MAPS_WORLD:8/unsigned>>,
             werld_map:to_binary(Map)],
  io:format("~p sending map to ~p ~p~n", [erlang:localtime(), Client#client.socket, Payload]),
  gen_tcp:send(Client#client.socket, Payload).

send_message(Message, Sender, Recipient) ->
  Payload = [<<?WERLD_RESPONSE_TYPE_MESSAGE>>,
             <<(length(binary_to_list(Message))):4/native-unit:8>>,
             werld_player:to_binary(Sender#client.player),
             Message],
  io:format("~p message: ~p~n", [erlang:localtime(), Message]),
  gen_tcp:send(Recipient#client.socket, Payload).
