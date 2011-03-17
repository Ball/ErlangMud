-module(erl_start).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
        eunit:test([player_tests, room_tests],[verbose]).
