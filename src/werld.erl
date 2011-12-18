-module(werld).

-export([start/0, stop/0]).

start() ->
  werld_sockserv:start_link().

stop() ->
  werld_sockserv:stop().
