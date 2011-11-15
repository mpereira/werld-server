-module(werld_client_sup).
-compile([export_all]).
-include("../include/client.hrl").
-include("../include/player.hrl").
-include("../include/response_types.hrl").

start_link() ->
  spawn_link(?MODULE, loop, [[]]).

loop(ClientList) ->
  receive
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
      NewClientList = [RegisteredClient | ClientList],
      werld_client_list:multicast_player_list_except(RegisteredClient,
                                                     NewClientList),
      Payload = werld_player:to_binary(RegisteredClient#client.player),
      Data = <<?WERLD_RESPONSE_TYPE_REGISTER:4/native-unit:8, Payload/binary>>,
      ClientSocket = Client#client.socket,
      io:format("~p unicasting ~B bytes to ~p ~p~n",
                [erlang:localtime(),
                 length(binary_to_list(Data)),
                 ClientSocket,
                 Data]),
      gen_tcp:send(ClientSocket, Data),
      loop(NewClientList);
    {unregister, Client} ->
      io:format("~p unregistering ~s~n",
                [erlang:localtime(), Client#client.player#player.name]),
      NewClientList = werld_client_list:delete(Client, ClientList),
      werld_client_list:multicast_player_list(NewClientList),
      loop(NewClientList);
    {players, Socket} ->
      io:format("~p players ~w~n", [erlang:localtime(), Socket]),
      PlayerList = werld_client_list:player_list(ClientList),
      Payload = werld_player_list:to_binary(PlayerList),
      PlayerListLength = length(PlayerList),
      Data = <<?WERLD_RESPONSE_TYPE_PLAYERS:4/native-unit:8,
               PlayerListLength:4/native-unit:8,
               Payload/binary>>,
      io:format("~p sending ~B bytes to ~p ~p~n",
                [erlang:localtime(), length(binary_to_list(Data)), Socket, Data]),
      gen_tcp:send(Socket, Data),
      loop(ClientList);
    {player, Client} ->
      io:format("~p player ~w~n", [erlang:localtime(), Client#client.socket]),
      % FIXME: update player state more intelligently.
      NewClientList = [Client | werld_client_list:delete(Client, ClientList)],
      werld_client_list:multicast_player_list(NewClientList),
      loop(NewClientList);
    {message, Client, Message} ->
      io:format("~p message from ~w ~s~n",
                [erlang:localtime(), Client#client.socket, Message]),
      werld_client_list:multicast_message(Message, Client, ClientList),
      loop(ClientList);
    stop ->
      stop()
  end.

stop() ->
    ok.
