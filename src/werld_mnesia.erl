%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_mnesia.

-module(werld_mnesia).
-author('Murilo Pereira <murilo@murilopereira.com>').

-define(WERLD_MNESIA_TABLES, [player]).
-define(WERLD_MNESIA_TIMEOUT, 5000).
-define(WERLD_MNESIA_CREATE_TABLE(Table),
        case mnesia:create_table(Table,
                                 [{attributes, record_info(fields, Table)},
                                  {disc_copies, [node()]}]) of
          {atomic, ok} -> ok;
          {aborted, {already_exists, Table}} -> ok
        end).

-include("../include/player.hrl").

-export([create_schema/1, create_tables/0]).

create_schema(Nodes) ->
  case mnesia:create_schema(Nodes) of
    ok -> ok;
    {error, {_, {already_exists, _}}} -> ok
  end.

create_tables() ->
  ?WERLD_MNESIA_CREATE_TABLE(player),
  ok = mnesia:wait_for_tables(?WERLD_MNESIA_TABLES, ?WERLD_MNESIA_TIMEOUT).
