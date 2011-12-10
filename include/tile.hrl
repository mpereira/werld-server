-record(tile, {type, properties}).

-define(WERLD_TILE_TYPE_DIRT,  0).
-define(WERLD_TILE_TYPE_GRASS, 1).
-define(WERLD_TILE_TYPE_SAND,  2).
-define(WERLD_TILE_TYPE_SNOW,  3).
-define(WERLD_TILE_TYPE_WATER, 4).
-define(WERLD_TILE_TYPE_STONE, 4).
-define(WERLD_TILE_TYPE_WALL,  6).

-define(WERLD_TILE_PROPERTY_TRAVERSABLE, 2#00000001).
