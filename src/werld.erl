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
  ensure_started(mnesia),
  werld_mnesia_tables:init(),
  werld_httpserv:start(),
  werld_sockserv:start().

start_link() ->
  ensure_started(mnesia),
  werld_mnesia_tables:init(),
  werld_httpserv:start_link(),
  werld_sockserv:start_link().

stop() ->
  Return = werld_sockserv:stop(),
  application:stop(werld_httpserv),
  application:stop(mnesia),
  Return.
