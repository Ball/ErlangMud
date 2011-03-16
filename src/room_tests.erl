-module(room_tests).
-include("tests.hrl").

room_test_() -> [
        ?Describe("Describe a room",
                ?It("Should describe the kitchen",
                    fun setup/0,
                    fun cleanup/1,
                    ?_test(begin ?assertEqual("Kitchen\nIt's a damn kitchen!",
                                              room:describe(room_kitchen)) end))),
        ?Describe("Searching For Room Information",
                [ ?It("Should find an existing room",
                        ?_test(begin ?assert(room:does_exist(room_kitchen)) end)),
                  ?It("Should not find an existing room",
                        ?_test(begin ?assertNot(room:does_exist(room_noplace)) end))
                ])
].

setup() ->
        room:start_room(room_kitchen).
cleanup(_Pid) ->
        room:stop_room(room_kitchen),
        true.
