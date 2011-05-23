-module(registry).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([add_player/3, remove_player/1, players/0, players_in_room/1, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
start() ->
  gen_server:start_link({global,?MODULE}, ?MODULE, [[]], []).
add_player(Nic, Location, Pid) ->
  gen_server:call({global,?MODULE}, {add_player, {Nic, Location, Pid}}).
remove_player(Nic) ->
  gen_server:call({global,?MODULE}, {remove_player, Nic}).
players() ->
  gen_server:call({global,?MODULE}, players).
players_in_room(Room) ->
  gen_server:call({global,?MODULE}, {players_in_room, Room}).

%% gen_server
init([State]) ->
  {ok, State}.

handle_call({add_player, {Nic,Room,Pid}}, _From, Players) ->
  OtherPlayers = lists:filter(fun ({PNic,_Room, _Pid}) -> PNic /= Nic end, Players),
  {reply, ok, [{Nic,Room,Pid} | OtherPlayers]};

handle_call({remove_player, Nic}, _From, Players) ->
  {reply, ok, (lists:filter(fun ({PNic,_Room, _Pid}) -> PNic /= Nic end, Players))};

handle_call({players_in_room, Room}, _From, Players) ->
  Members =  (lists:filter(fun ({_Nick, PRoom, _Pid}) -> PRoom =:= Room end, Players)),
  {reply, {ok, Members}, Players};

handle_call(players, _From, Players) ->
  {reply, {ok, Players}, Players}.

handle_cast(stop, Players) ->
  {stop, normal, Players}.

handle_info(_info, Players) ->
  {noreply, Players}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
