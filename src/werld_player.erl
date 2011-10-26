-module(werld_player).
-compile([export_all]).
-include("../include/player.hrl").

to_binary(#player{id = Id, name = Name, y = Y, x = X}) ->
  <<Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>.
