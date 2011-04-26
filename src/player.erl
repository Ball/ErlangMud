-module(player).
-export([create_player/3, select_player/1]).
-export([start_player/1, does_exist/1, describe/1, stop_player/1]).

-include("src/em_records.herl").

create_player(Key, Name, Description) ->
  mnesia:transaction(fun() ->
    mnesia:write( #player{key=Key,name=Name,description=Description}) end).
select_player(Key) ->
  {atomic, [Row]} = mnesia:transaction(
    fun() -> mnesia:read({player, Key}) end),
  Row.
does_exist(Key) ->
  {atomic, Rows} = mnesia:transaction(fun() -> mnesia:read({player, Key}) end),
  case Rows of
    [] -> false;
    _Any -> true
  end.
location(Key) ->
  {atomic, [Row]} = mnesia:transaction( fun() -> mnesia:read({player, Key}) end),
  Row#player.location


start_player(Key) ->
  em_player:start_link(select_player(Key)).
stop_player(Key) ->
  em_player:stop(Key).
describe(Key) ->
  {ok, Description} = em_player:describe(Key),
  Description.
