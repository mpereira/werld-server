%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld.

-module(werld).
-author('Murilo Pereira <murilo@murilopereira.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

start() ->
  werld_mnesia:create_schema([node()]),
  ensure_started(mnesia),
  werld_mnesia:create_tables(),
  HttpServReturn = werld_httpserv:start(),
  SockServReturn = werld_sockserv:start(),
  EvServReturn = werld_evserv:start(),
  [{sockserv, SockServReturn},
   {evserv, EvServReturn},
   {httpserv, HttpServReturn}].

start_link() ->
  werld_mnesia:create_schema([node()]),
  ensure_started(mnesia),
  werld_mnesia:create_tables(),
  HttpServReturn = werld_httpserv:start_link(),
  SockServReturn = werld_sockserv:start_link(),
  EvServReturn = werld_evserv:start_link(),
  [{sockserv, SockServReturn},
   {evserv, EvServReturn},
   {httpserv, HttpServReturn}].

stop() ->
  EvServReturn = werld_evserv:stop(),
  SockServReturn = werld_sockserv:stop(),
  HttpServReturn = application:stop(werld_httpserv),
  application:stop(mnesia),
  [{sockserv, SockServReturn},
   {evserv, EvServReturn},
   {httpserv, HttpServReturn}].
