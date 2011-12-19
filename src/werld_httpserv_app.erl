%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc Callbacks for the werld_httpserv application.

-module(werld_httpserv_app).
-author('Murilo Pereira <murilo@murilopereira.com>').

-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for werld.
start(_Type, _StartArgs) ->
  werld_httpserv_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for werld.
stop(_State) ->
  ok.
