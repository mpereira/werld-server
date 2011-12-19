%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_user_resource.

-module(werld_user_resource).
-author('Murilo Pereira <murilo@murilopereira.com>').

-export([init/1,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Config) ->
  {ok, undefined}.

to_json(ReqData, Context) ->
  {mochijson:encode({struct, {user, name = "user"}}), ReqData, Context}.
