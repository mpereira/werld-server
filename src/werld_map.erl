-module(werld_map).
-compile([export_all]).
-include("../include/map.hrl").
-include("../include/tile.hrl").

% TODO: we're currently ignoring the map type. In the future we'll have more
% than one type of map, so we'll need to build different maps.
build(_) ->
  Width = 80,
  Height = 22,
  GrassTiles = array:new(Width * Height,
                         {default, <<?WERLD_TILE_TYPE_GRASS:8/unsigned,
                                     ?WERLD_TILE_PROPERTY_TRAVERSABLE:8/unsigned>>}),
  WallTileIndexes = lists:seq(0, Height - 1) ++
                      lists:seq((Width - 1) * Height, Width * Height - 1) ++
                      lists:seq(Height, (Width - 1) * Height - 1, Height) ++
                      lists:seq(2 * Height - 1, (Width - 1) * Height - 1, Height),
  % TODO: MY EYES.
  DirtTileIndexes =
    lists:seq(4 + ((Width - 20) * Height), 6 + ((Width - 20) * Height)) ++
    lists:seq(3 + ((Width - 19) * Height), 7 + ((Width - 19) * Height)) ++
    lists:seq(3 + ((Width - 18) * Height), 7 + ((Width - 18) * Height)) ++
    lists:seq(3 + ((Width - 17) * Height), 8 + ((Width - 17) * Height)) ++
    lists:seq(3 + ((Width - 16) * Height), 8 + ((Width - 16) * Height)) ++
    lists:seq(2 + ((Width - 15) * Height), 8 + ((Width - 15) * Height)) ++
    lists:seq(2 + ((Width - 14) * Height), 8 + ((Width - 14) * Height)) ++
    lists:seq(2 + ((Width - 13) * Height), 7 + ((Width - 13) * Height)) ++
    lists:seq(2 + ((Width - 12) * Height), 7 + ((Width - 12) * Height)) ++
    lists:seq(3 + ((Width - 11) * Height), 7 + ((Width - 11) * Height)) ++
    lists:seq(3 + ((Width - 10) * Height), 6 + ((Width - 10) * Height)) ++
    lists:seq(4 + ((Width - 9) * Height), 6 + ((Width - 9) * Height)) ++
    lists:seq(4 + ((Width - 8) * Height), 5 + ((Width - 8) * Height)),
  TilesWithWalls =
    array:map(fun(Index, Tile) ->
                case lists:member(Index, WallTileIndexes) of
                  true -> <<?WERLD_TILE_TYPE_WALL:8/unsigned, 0:8/unsigned>>;
                  false -> Tile
                end
              end, GrassTiles),
  TilesWithDirts =
    array:map(fun(Index, Tile) ->
                case lists:member(Index, DirtTileIndexes) of
                  true -> <<?WERLD_TILE_TYPE_DIRT:8/unsigned,
                            ?WERLD_TILE_PROPERTY_TRAVERSABLE:8/unsigned>>;
                  false -> Tile
                end
              end, TilesWithWalls),
  #map{width = Width, height = Height, tiles = TilesWithDirts}.

to_binary(Map) ->
  <<(Map#map.width):32/integer-little,
    (Map#map.height):32/integer-little,
    (binary:list_to_bin(array:to_list(Map#map.tiles)))/binary>>.
