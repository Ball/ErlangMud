-module(room_tests).
-include("tests.hrl").

room_test_() -> [
 ?Describe("Taking an item from the room",
   [?It("should remove it from the room", fun setup/0, fun cleanup/1,
        begin
        ok = room:add_to_room(kitchen, "a wrench"),
        {ok, Item} = room:take_from_room(kitchen, "a wrench"),
        {ok, Description} = room:describe(kitchen),
        ?assertEqual("a wrench", Item),
        ?assertEqual("It's a kitchen",Description) end),
    ?It("should handle an item not in the list", fun setup/0, fun cleanup/1,
        begin
        ok = room:add_to_room(kitchen, "a wombat"),
        {not_found, Item} = room:take_from_room(kitchen, "a wrench"),
        ?assertEqual("a wrench", Item) end),
    ?It("should handle an empty list", fun setup/0, fun cleanup/1,
        begin
        {not_found, Item} = room:take_from_room(kitchen, "a wrench"),
        ?assertEqual("a wrench", Item) end)
 ]),
 ?Describe("Adding an item to the room",
   [?It("should show in the description", fun setup/0, fun cleanup/1,
        begin
	ok = room:add_to_room(kitchen, "a wrench"),
        {ok,Description} = room:describe(kitchen),
        ?assertEqual("It's a kitchen\n\ta wrench",Description)end),
    ?It("should show multiple items", fun setup/0, fun cleanup/1,
        begin
        ok = room:add_to_room(kitchen, "a wrench"),
        ok = room:add_to_room(kitchen, "a wombat"),
        {ok,Description} = room:describe(kitchen),
        ?assertEqual("It's a kitchen\n\ta wrench\n\ta wombat",Description)end)
   ]) 
].

setup()-> io:format(""),stubs:fake_rooms().
cleanup(_Pid) -> stubs:stop_fake_rooms(), true.
