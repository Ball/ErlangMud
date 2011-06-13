-module(room_tests).
-include("../src/records.hrl").
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

room_database_test_() -> [
  ?Describe("run a room from the database",
    [?It("should load a stable from the database",
         fun insert_stable/0, fun delete_stable/1,
         begin
         room:start_from_db(stable),
         {ok,Description} = room:describe(stable),
         ?assertEqual("A stable from the database",Description)
         end)
  ]),
  ?Describe("run all rooms in the database",
    [?It("should start multiple rooms",
         fun insert_stable/0, fun delete_stable/1,
         begin
         room:start_from_db(),
         {ok,Description1} = room:describe(stable),
         {ok,Description2} = room:describe(yard),
         ?assertEqual("A stable from the database", Description1),
         ?assertEqual("A yard from the database", Description2)
         end)
  ])
].

insert_stable() ->
        mnesia:transaction(fun() ->
                mnesia:write(
                 #room{key=yard,
                       name="The horse yard",
                       description="A yard from the database"}
                ),
                mnesia:write(
                 #room{key=stable,
                       name="The king's stable",
                       description="A stable from the database"}
                )
                end).
delete_stable(_Pid) ->
        gen_server:cast({global, stable}, stop),
        mnesia:transaction(fun() ->
                           mnesia:delete(room, yard),
                           mnesia:delete(room, stable) end),
        true.

setup()-> io:format(""),stubs:fake_rooms().
cleanup(_Pid) -> stubs:stop_fake_rooms(), true.
