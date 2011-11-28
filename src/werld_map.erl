-module(werld_map).
-compile([export_all]).
-include("../include/map.hrl").

% TODO: we're currently ignoring the map type. In the future we'll have more
% than one type of map, so we'll need to build different maps.
build(_) ->
  Width = 60,
  Height = 20,
  Tiles = array:new(Width * Height, {default, 0}),
  #map{width = Width, height = Height, tiles = Tiles}.

to_binary(Map) ->
  <<(Map#map.width):32/integer-little,
    (Map#map.height):32/integer-little,
    (binary:list_to_bin(array:to_list(Map#map.tiles)))/binary>>.
