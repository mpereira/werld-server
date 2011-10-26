-module(werld_player_list).
-compile([export_all]).

to_binary(PlayerList) ->
  BinaryPlayerList = lists:map(fun werld_player:to_binary/1, PlayerList),
  lists:foldl(fun(PlayerBinary, Binary) -> <<Binary/binary, PlayerBinary/binary>> end,
              <<>>,
              BinaryPlayerList).
