-module(test_runner).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
        eunit:test([player_tests, room_tests],[verbose]).
