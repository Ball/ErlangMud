-module(room).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([start_room/4,start_room/3, add_to_room/2, take_from_room/2, describe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_room(Key,Name,Description,Exits) ->
        Room = #room{key=Key,name=Name,description=Description,exits=Exits},
        gen_server:start_link({local,Key}, ?MODULE,[Room], []).

start_room(Key,Name,Description) ->
        Room = #room{key=Key,name=Name,description=Description},
        gen_server:start_link({local,Key}, ?MODULE, [Room], []).

add_to_room(RoomName,Item) ->
  gen_server:call(RoomName, {add_item, Item}).

take_from_room(RoomName,Item) ->
  gen_server:call(RoomName, {take_item, Item}).

describe(RoomName) ->
  gen_server:call(RoomName,describe).

%% gen_server
init([Room]) ->
        {ok, Room}.

% direction
handle_call({direction,Direction},_From,State)->
  Directions = lists:filter(fun (E) -> E#room_exit.direction == Direction end, State#room.exits),
    case Directions of
      []  -> {reply, not_found, State};
      [E] -> {reply, {ok, E#room_exit.location_key},State};
      _   -> {reply, too_many, State}
    end;

% add item to the room
handle_call({add_item, Item}, _From, State) ->
        {reply, ok, State#room{ items = [Item | State#room.items] }};

% take an item from the room
handle_call({take_item, Item}, _From, State) ->
  IsMember = lists:member(Item, (State#room.items)),
  if
    IsMember ->
      Response = ok;
    true     ->
      Response = not_found
  end,
  {reply, {Response, Item}, State#room{ items = lists:delete(Item, (State#room.items)) }};

% describe the room
handle_call(describe, _From, State) ->
  Description = string:join([(State#room.description) | lists:reverse(State#room.items)], "\n\t"),
  {reply, {ok, Description}, State}.

% stop the room
handle_cast(stop, State) ->
        {stop, normal, State}.

handle_info(_Info,State) ->
        {noreply,State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
