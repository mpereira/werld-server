%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_player.

-module(werld_player).
-author('Murilo Pereira <murilo@murilopereira.com>').

-include("../include/player.hrl").

-export([to_binary/1]).

to_binary(#player{id = Id, name = Name, y = Y, x = X}) ->
  <<Id:4/bytes, Name:20/bytes, Y:4/bytes, X:4/bytes>>.
