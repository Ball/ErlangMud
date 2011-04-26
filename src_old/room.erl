-module(room).
-export([create_room/3, select_room/1]).
-export([start_room/1, does_exist/1, describe/1, stop_room/1]).
-include("src/em_records.herl").

create_room(Key, Name, Description) ->
  mnesia:transaction(fun() ->
    mnesia:write( #room{key=Key,name=Name,description=Description}) end).

select_room(Key) ->
  {atomic, [Row]} = mnesia:transaction(
    fun() -> mnesia:read({room, Key}) end),
  Row.

does_exist(Key) ->
  {atomic, Rows} = mnesia:transaction(fun() -> mnesia:read({room, Key}) end),
  case Rows of
          [] -> false;
          _Any -> true
  end.

start_room(Key) ->
  em_room:start_link(select_room(Key)).
stop_room(Key) ->
  em_room:stop(Key).
describe(Key) ->
   {ok, Description} = em_room:describe(Key),
   Description.
