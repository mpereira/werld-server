%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc Supervisor for the werld_httpserv application.

-module(werld_httpserv_sup).
-author('Murilo Pereira <murilo@murilopereira.com>').

-behaviour(supervisor).
-export([start_link/0, upgrade/0]).

-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old =
    sets:from_list([Name ||
                    {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun(Id, ok) ->
              supervisor:terminate_child(?MODULE, Id),
              supervisor:delete_child(?MODULE, Id),
              ok
            end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} =
    file:consult(filename:join([filename:dirname(code:which(?MODULE)),
                                "..",
                                "priv",
                                "dispatch.conf"])),
    WebConfig = [{ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent,
           5000,
           worker,
           [mochiweb_socket_server]},
    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.