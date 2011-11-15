-module(werld_client).
-compile([export_all]).
-include("include/player.hrl").
-include("include/client.hrl").
-include("include/response_types.hrl").

send_message(Message, Sender, Recipient) ->
  Payload = [<<?WERLD_RESPONSE_TYPE_MESSAGE:4/native-unit:8>>,
             <<(length(binary_to_list(Message))):4/native-unit:8>>,
             werld_player:to_binary(Sender#client.player),
             Message],
  io:format("~p message: ~p~n", [erlang:localtime(), Message]),
  gen_tcp:send(Recipient#client.socket, Payload).
