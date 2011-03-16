-module(em_room).
-behavior(gen_server).
-include("em_records.herl").

%% API
-export([start_link/1,
         describe/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API

start_link(Room) ->
        gen_server:start_link({local, Room#room.key}, ?MODULE, [Room] , []).
describe(Key) ->
        gen_server:call(Key, describe).
stop(Key) ->
        gen_server:call(Key, stop).

%% gen_server
init([Room]) ->
        {ok, Room}.

handle_call(describe, _From, State) ->
        Description = lists:flatten( io_lib:format("~s~n~s",[State#room.name, State#room.description])), 
        {reply, {ok, Description}, State};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast(stop, State) ->
        {stop, normal, State};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.



