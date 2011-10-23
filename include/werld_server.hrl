-define(LISTEN_PORT, 9876).
-define(TCP_OPTIONS, [binary,
                      {packet, raw},
                      {nodelay, true},
                      {reuseaddr, true},
                      {active, once}]).
