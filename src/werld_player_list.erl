%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_player_list.

-module(werld_player_list).
-author('Murilo Pereira <murilo@murilopereira.com>').

-export([to_binary/1]).

to_binary(PlayerList) ->
  BinaryPlayerList = lists:map(fun werld_player:to_binary/1, PlayerList),
  lists:foldl(fun(PlayerBinary, Binary) -> <<Binary/binary, PlayerBinary/binary>> end,
              <<>>,
              BinaryPlayerList).
