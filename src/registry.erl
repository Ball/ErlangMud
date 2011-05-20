-module(registry).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([add_player/3, players/0, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
start() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [[]], []).
add_player(Nic, Location, Pid) ->
  gen_server:call(?MODULE, {add_player, {Nic, Location, Pid}}).
players() ->
  gen_server:call(?MODULE, players).

%% gen_server
init([State]) ->
  {ok, State}.

handle_call({add_player, Player}, _From, Players) ->
  {reply, ok, [Player | Players]};
handle_call(players, _From, Players) ->
  {reply, {ok, Players}, Players}.

handle_cast(stop, Players) ->
  {stop, normal, Players}.

handle_info(_info, Players) ->
  {noreply, Players}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
