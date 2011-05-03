-module(player).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([login/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

login(Username,"Hello") ->
  Player = #player{key=Username,name=Username,description="Stuff",location_key=lobby},
  {ok,Pid} = gen_server:start_link(?MODULE, [Player], []),
  player_proxy:new(Pid);
login(_Username,_Password) ->
  error.

%% gen_server
init([Player]) ->
        {ok, Player}.
handle_call(look, _From, State) ->
        {ok, Description} = gen_server:call(State#player.location_key, describe),
        {reply, {ok, Description}, State};
handle_call({look, Item}, _From, State) ->
        {reply, {ok, io_lib:format("It's a ~w",[Item])},State};
handle_call({move,Direction}, _From, State) ->
        {ok, LocationKey} = gen_server:call(State#player.location_key,{direction,Direction}),
        NewState = State#player{location_key=LocationKey},
        {reply, {ok, LocationKey}, NewState};
handle_call(inventory, _From, State) ->
        {reply, {ok, "You aint got jack!"}, State};
handle_call(_Atom, _From, State) ->
        {reply, {ok, "Blah!"}, State}.
handle_cast(stop, State)->
        {stop, normal, State};
handle_cast(_Msg, State) ->
        {noreply, State}.
handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.
code_change(_OldVsn, State, _Extra) ->
        {ok,State}.
