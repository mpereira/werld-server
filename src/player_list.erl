-module(player_list).
-export([loop/1, start/1, stop/0, add/2, remove/2]).
-record(player, { name }).
-define(TIMEOUT, 4000).

start(PlayerList) ->
  Pid = spawn(?MODULE, loop, [PlayerList]),
  {ok, Pid}.

loop(PlayerList) ->
  receive
    { From, { add, Player } } ->
      From ! { self(), { ok, Player } },
      loop([Player | PlayerList]);
    { From, { remove, Player } } ->
      case lists:member(Player, PlayerList) of
        true ->
          From ! { self(), { ok, Player } },
          loop(lists:delete(Player, PlayerList));
        false ->
          From ! { self(), not_found },
          loop(PlayerList)
      end
  end.

add(Pid, Player) ->
  Pid ! { self(), { add, Player } },
  receive
    { Pid, Message } -> Message
  end.

remove(Pid, Player) ->
  Pid ! { self(), { remove, Player } },
  receive
    { Pid, Message } -> Message
  end.

stop() ->
    ok.
