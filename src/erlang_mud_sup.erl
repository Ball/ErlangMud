-module(erlang_mud_sup).
-behavior(supervisor).
-include("src/records.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	io:format("supervisor start_link"),
        supervisor:start_link({local, ?SERVER},?MODULE,[]).
init([]) ->
	io:format("I'm starting the supervisor!"),
        Server = {registry, %internal id
                  {registry, start, []}, % start call
                  permanent, %restart
                  2000, %shutdown
                  worker, %type
                  [registry]}, %
	Rooms = room:get_rooms(),
	RoomDefs = lists:map( fun(R) -> {R#room.key, %internal id
					 {room, start_from_db, [R#room.key]}, %start call
					 permanent, %restart
					 2000, %shutdown
					 worker,
					 [room]} end, Rooms ),
        Children = [Server | RoomDefs],
        RestartStrategy = {one_for_one, 0, 1},
        {ok, {RestartStrategy, Children}}.
