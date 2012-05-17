-module(player).
-include("src/records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-behavior(gen_server).

%% API
-export([login/2, get_players/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% login 
login(Username,Password) ->
   Result = mnesia:transaction(fun()->
         qlc:eval( qlc:q([X || X<- mnesia:table(player),
                               X#player.name =:= Username,
                               X#player.password =:= Password])) end),
   case Result of
     {atomic, [Player]} ->
                {ok,Pid} = gen_server:start_link(?MODULE, [Player#player{location_key=lobby}], []),
                registry:add_player(Username, lobby, Pid),
                player_proxy:new(Pid);
     _Else -> error
   end.
get_players() ->
   {atomic, Rows} = mnesia:transaction(fun() ->
      qlc:eval( qlc:q([X || X <- mnesia:table(player)])) end),
   lists:map(fun(P) -> {P#player.name, P#player.password} end, Rows).

%% gen_server
init([Player|_]) ->
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
        {ok, LocationKey} = room:direction(State#player.location_key,Direction),
        registry:add_player(State#player.name, LocationKey, self()),
        NewState = State#player{location_key=LocationKey},
        {reply, {ok, LocationKey}, NewState};

% take item
handle_call({take, Item}, _From, State) ->
        {Status,Item} = room:take_from_room(State#player.location_key, Item),
        case Status of
          ok -> NewItems = [Item | State#player.items];
          not_found -> NewItems = State#player.items
        end,
        NewState = State#player{items = NewItems},
        mnesia:transaction(fun() -> mnesia:write(NewState) end),
        {reply, {Status, Item}, NewState};

% drop an item
handle_call({drop, Item}, _From,State) ->
        case lists:member(Item, (State#player.items)) of
          true -> NewItems = lists:delete(Item, (State#player.items)),
                  room:add_to_room(State#player.location_key, Item),
                  NewState = State#player{items = NewItems},
                  mnesia:transaction(fun()->mnesia:write(NewState) end),
                  {reply, {ok, Item}, NewState};
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

% tell
handle_call({tell, Nic, Message}, _From, State) ->
        {ok,Players} = registry:players(),
        PickedNics = lists:filter(fun ({PNic, _Room, _Pid}) -> PNic =:= Nic end, Players),
        lists:map(fun ({_Nic, _Room, Pid}) -> gen_server:cast(Pid, {shout, Nic, Message}) end,
                  PickedNics),
        {reply, ok, State};

% whisper
handle_call({whisper, Nic, Message}, _From, State) ->
        {ok,Players} = registry:players_in_room(State#player.location_key),
        [{_Nic,_Room,SpokenPid}] = lists:filter(fun({PNic, _Room1, _Pid}) -> PNic =:= Nic end, Players),
        OtherPlayers = lists:filter(fun({PNic, _Room1, _Pid}) -> (PNic /= Nic) and (PNic /= (State#player.name)) end, Players),
        gen_server:cast(SpokenPid, {whisper, (State#player.name),Message}),
        lists:map(fun ({_Nic1,_Room1, Pid}) -> gen_server:cast(Pid, {mumble, (State#player.name), Nic}) end,
                  OtherPlayers),
        {reply, ok, State};


% shout
handle_call({shout, Message}, _From, State) ->
        Nic = State#player.name,
        {ok, Players} = registry:players(),
        lists:map(fun ({_Nic1, _Room1, Pid}) -> gen_server:cast(Pid, {shout,Nic, Message}) end,
                  lists:filter(fun ({PNic, _Room, _Pid}) -> PNic /= Nic end,
                               Players)),
        {reply, ok, State};

% say
handle_call({say, Message}, _From, State) ->
        Nic = State#player.name,
        Room = State#player.location_key,
        {ok, Players} = registry:players_in_room(Room),
        lists:map(fun ({_Nic, _Room, Pid}) -> gen_server:cast(Pid,{say,Nic, Message}) end,
                  lists:filter(fun ({PNic, _Room, _Pid})-> PNic /= Nic end,
                               Players)),
        {reply, ok, State}.

handle_cast({mumble,From,To}, State) ->
        io:format("~s whispered to ~s~n", [From, To]),
        {noreply,State};
handle_cast({say, Nic, Message}, State)->
        io:format("~s says ~s~n", [Nic, Message]),
        {noreply, State};
handle_cast({shout, Nic, Message}, State) ->
        io:format("~s shouts ~s~n", [Nic, Message]),
        {noreply, State};
handle_cast(stop, State)->
        {stop, normal, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok,State}.
