-module(erlang_mud_sup).
-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
        supervisor:start_link({local, ?SERVER},?MODULE,[]).
init([]) ->
        Server = {registry, %internal id
                  {registry, start, []}, % start call
                  permanent, %restart
                  2000, %shutdown
                  worker, %type
                  [registry]}, %
        Children = [Server],
        RestartStrategy = {one_for_one, 0, 1},
        {ok, {RestartStrategy, Children}}.
