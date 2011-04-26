-module(erl_start).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
        room_tests:module_setup(),
        player_tests:module_setup(),
        eunit:test([player_tests, room_tests],[verbose]).
