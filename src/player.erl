-module(player).
-include("src/records.hrl").
-behavior(gen_server).

%% API
-export([login/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% login : TODO: store and use real player passwords in DB
login(Username,"Hello") ->
  Player = #player{key=Username,name=Username,description="Stuff",location_key=lobby},
  {ok,Pid} = gen_server:start_link(?MODULE, [Player], []),
  registry:add_player(Username,lobby,Pid),
  player_proxy:new(Pid);
login(_Username,_Password) ->
  error.

%% gen_server
init([Player]) ->
        {ok, Player}.
% look
handle_call(look, _From, State) ->
        {ok, Description} = room:describe(State#player.location_key),
        {reply, {ok, Description}, State};

% look at item
handle_call({look, Item}, _From, State) ->
        {reply, {ok, io_lib:format("It's a ~w",[Item])},State};

% move through exit
handle_call({move,Direction}, _From, State) ->
        {ok, LocationKey} = gen_server:call(State#player.location_key,{direction,Direction}),
        NewState = State#player{location_key=LocationKey},
        {reply, {ok, LocationKey}, NewState};

% take item
handle_call({take, Item}, _From, State) ->
        {Status,Item} = room:take_from_room(State#player.location_key, Item),
        case Status of
          ok -> NewItems = [Item | State#player.items];
          not_found -> NewItems = State#player.items
        end,
        {reply, {Status, Item}, State#player{items = NewItems}};

% drop an item
handle_call({drop, Item}, _From,State) ->
        case lists:member(Item, (State#player.items)) of
          true -> NewItems = lists:delete(Item, (State#player.items)),
                  room:add_to_room(State#player.location_key, Item),
                  {reply, {ok, Item}, State#player{items = NewItems}};
          _    -> {reply, {not_found, Item}, State}
        end;

% inventory
handle_call(inventory, _From, State) ->
        case State#player.items of
          [] -> Message = "You aint got jack!";
          _  -> Message = lists:flatten( string:join(["You have" | State#player.items], "\n\t"))
        end,
        {reply, {ok, Message}, State};

% who
handle_call(who, _From, State) ->
        {ok, Players} = registry:players(),
        PlayerLines = lists:map( fun ({Nic, Room, _Pid}) ->
                                     Nic ++ " : " ++ atom_to_list(Room)  end, Players),
        String = "The players logged in are\n" ++
                 lists:flatten(string:join(PlayerLines, "\n")),
        {reply, {ok,String}, State};

handle_call({say, Message}, _From, State) ->
        Nic = State#player.name,
        Room = State#player.location_key,
        {ok, Players} = registry:players_in_room(Room),
        lists:map(fun ({_Nic, _Room, Pid}) -> gen_server:cast(Pid,{say,Nic, Message}) end,
                  lists:filter(fun ({PNic, _Room, _Pid})-> PNic /= Nic end,
                               Players)),
        {reply, ok, State}.

handle_cast({say, Nic, Message}, State)->
        io:format("~s says ~s~n", [Nic, Message]),
        {noreply, State};
handle_cast(stop, State)->
        {stop, normal, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok,State}.
