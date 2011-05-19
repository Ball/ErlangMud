-module(room_tests).
-include("tests.hrl").

room_test_() -> [
 ?Describe("Adding an item to the room",
   [?It("should show in the description", fun setup/0, fun cleanup/1,
        ?_test(begin
	       ok = room:add_to_room(kitchen, "a wrench"),
               {ok,Description} = room:describe(kitchen),
               ?assertEqual("It's a kitchen\n\ta wrench",Description)end)),
    ?It("should show multiple items", fun setup/0, fun cleanup/1,
        ?_test(begin
               ok = room:add_to_room(kitchen, "a wrench"),
               ok = room:add_to_room(kitchen, "a wombat"),
               {ok,Description} = room:describe(kitchen),
               ?assertEqual("It's a kitchen\n\ta wrench\n\ta wombat",Description)end))
   ])
].

setup()-> io:format(""),stubs:fake_rooms().
cleanup(_Pid) -> stubs:stop_fake_rooms(), true.
