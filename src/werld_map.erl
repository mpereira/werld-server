-module(werld_map).
-compile([export_all]).
-include("../include/map.hrl").

% TODO: we're currently ignoring the map type. In the future we'll have more
% than one type of map, so we'll need to build different maps.
build(_) ->
  Width = 80,
  Height = 22,
  EmptyTiles = array:new(Width * Height, {default, ?WERLD_MAP_GROUND}),
  WallTileIndexes = lists:seq(0, Height - 1) ++
                      lists:seq((Width - 1) * Height, Width * Height - 1) ++
                      lists:seq(Height, (Width - 1) * Height - 1, Height) ++
                      lists:seq(2 * Height - 1, (Width - 1) * Height - 1, Height),
  Tiles = array:map(fun(Index, Value) ->
                      case lists:member(Index, WallTileIndexes) of
                        true -> ?WERLD_MAP_WALL;
                        false -> Value
                      end
                    end, EmptyTiles),
  #map{width = Width, height = Height, tiles = Tiles}.

to_binary(Map) ->
  <<(Map#map.width):32/integer-little,
    (Map#map.height):32/integer-little,
    (binary:list_to_bin(array:to_list(Map#map.tiles)))/binary>>.
