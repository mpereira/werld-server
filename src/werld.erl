-module(werld).
-compile(export_all).

start() ->
  werld_sup:start_link().

stop() ->
  werld_sup:stop().
