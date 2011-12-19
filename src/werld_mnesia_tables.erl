%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_mnesia_tables.

-module(werld_mnesia_tables).
-author('Murilo Pereira <murilo@murilopereira.com>').

-define(WERLD_MNESIA_TABLES, [player]).
-define(WERLD_MNESIA_TIMEOUT, 5000).
-define(WERLD_MNESIA_CREATE_TABLE(Table),
        mnesia:create_table(Table, {attributes, record_info(fields, Table)})).

-include("../include/player.hrl").

-export([init/0]).

init() ->
  ?WERLD_MNESIA_CREATE_TABLE(player),
  mnesia:wait_for_tables(?WERLD_MNESIA_TABLES, ?WERLD_MNESIA_TIMEOUT).
