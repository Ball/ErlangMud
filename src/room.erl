-module(room).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([start_room/4,start_room/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_room(Key,Name,Description,Exits) ->
        Room = #room{key=Key,name=Name,description=Description,exits=Exits},
        gen_server:start_link({local,Key}, ?MODULE,[Room], []).

start_room(Key,Name,Description) ->
        Room = #room{key=Key,name=Name,description=Description},
        gen_server:start_link({local,Key}, ?MODULE, [Room], []).
%% gen_server
init([Room]) ->
        {ok, Room}.
handle_call({direction,Direction},_From,State)->
        Directions = lists:filter(fun (E) -> E#room_exit.direction == Direction end, State#room.exits),
        case Directions of
           [] -> {reply, not_found, State};
           [E] -> {reply, {ok, E#room_exit.location_key},State};
           _ -> {reply, to_many, State}
        end;
handle_call(describe, _From, State) ->
        {reply, {ok, State#room.description}, State}.
handle_cast(stop, State) ->
        {stop, normal, State}.
handle_info(_Info,State) ->
        {noreply,State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
