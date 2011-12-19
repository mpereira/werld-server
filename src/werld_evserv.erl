%% @author Murilo Pereira <murilo@murilopereira.com>
%% @copyright 2011 Murilo Pereira.

%% @doc werld_evserv.

-module(werld_evserv).
-author('Murilo Pereira <murilo@murilopereira.com>').

-include("../include/client.hrl").
-include("../include/map.hrl").
-include("../include/player.hrl").
-include("../include/response_types.hrl").

-export([start/0, start_link/0]).

-export([loop/1, stop/0]).

-record(state, {client_list=[], map=undefined}).

start() ->
  Map = werld_map:build(?WERLD_MAP_WORLD),
  io:format("~p built map ~p~n", [erlang:localtime(), Map]),
  spawn(?MODULE, loop, [#state{client_list=[], map=Map}]).

start_link() ->
  Map = werld_map:build(?WERLD_MAP_WORLD),
  io:format("~p built map ~p~n", [erlang:localtime(), Map]),
  spawn_link(?MODULE, loop, [#state{client_list=[], map=Map}]).

loop(#state{} = State) ->
  receive
    {authenticate, Client} ->
      io:format("~p authenticate ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      % if player = Player.find_by_account(account)
      % else
      %   Player doesn't exist.
      % end
      loop(State);
    {register, Client} ->
      io:format("~p registering ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      % FIXME: temporary hack to assign an unique identifier and initial
      % position coordinates to players. This will be persisted in a database
      % in the future.
      RegisteredClient =
        #client{socket = Client#client.socket,
                player =
                  Client#
                    client.player#
                    player{id = crypto:rand_bytes(4),
                           y = <<(random:uniform(10)):32/integer-little>>,
                           x = <<(random:uniform(10)):32/integer-little>>}},
      NewClientList = [RegisteredClient | State#state.client_list],
      werld_client_list:multicast_player_list_except(RegisteredClient,
                                                     NewClientList),
      Payload = werld_player:to_binary(RegisteredClient#client.player),
      Data = <<?WERLD_RESPONSE_TYPE_REGISTER, Payload/binary>>,
      ClientSocket = Client#client.socket,
      io:format("~p unicasting ~B bytes to ~p ~p~n",
                [erlang:localtime(),
                 length(binary_to_list(Data)),
                 ClientSocket,
                 Data]),
      gen_tcp:send(ClientSocket, Data),
      loop(State#state{client_list = NewClientList});
    {disconnect, Socket} ->
      Client =
        werld_client_list:find_client_by_socket(Socket, State#state.client_list),
      io:format("~p disconnect ~p~n", [erlang:localtime(), Socket]),
      NewClientList = werld_client_list:delete(Client, State#state.client_list),
      werld_client_list:multicast_player_list(NewClientList),
      loop(State#state{client_list = NewClientList});
    {players, Socket} ->
      io:format("~p players ~w~n", [erlang:localtime(), Socket]),
      PlayerList = werld_client_list:player_list(State#state.client_list),
      Payload = werld_player_list:to_binary(PlayerList),
      PlayerListLength = length(PlayerList),
      Data = <<?WERLD_RESPONSE_TYPE_PLAYERS,
               PlayerListLength:4/native-unit:8,
               Payload/binary>>,
      io:format("~p sending ~B bytes to ~p ~p~n",
                [erlang:localtime(), length(binary_to_list(Data)), Socket, Data]),
      gen_tcp:send(Socket, Data),
      loop(State);
    {player, Client} ->
      io:format("~p player ~w~n", [erlang:localtime(), Client#client.socket]),
      % FIXME: update player state more intelligently.
      NewClientList =
        [Client | werld_client_list:delete(Client, State#state.client_list)],
      werld_client_list:multicast_player_list(NewClientList),
      loop(State#state{client_list = NewClientList});
    {message, Client, Message} ->
      io:format("~p message from ~w ~s~n",
                [erlang:localtime(), Client#client.socket, Message]),
      werld_client_list:multicast_message(Message, Client, State#state.client_list),
      loop(State);
    {map, Socket, _Map} ->
      io:format("~p map requested from ~w~n", [erlang:localtime(), Socket]),
      Client =
        werld_client_list:find_client_by_socket(Socket, State#state.client_list),
      Map = State#state.map,
      werld_client:send_map(Map, Client),
      loop(State);
    stop ->
      stop()
  end.

stop() ->
    ok.
