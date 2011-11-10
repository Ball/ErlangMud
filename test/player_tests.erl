-module(player_tests).
-include("../src/records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("tests.hrl").

player_test_() ->[
  ?Describe("Bad Password",
    [?It("should return an error",fun setup/0,fun cleanup/1,
         begin
         ?assertEqual(error, player:login("Tony", "BadPassword"))end)
    ]),
  ?Describe("Good Password",
    [?It("should have a player proxy",fun setup/0,fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         ?assertEqual({ok,"You aint got jack!"},
                      Me:inventory()) end)
    ]),
  ?Describe("Room Interaction",
    [?It("should describe the lobby",fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         ?assertEqual({ok, "It's a lobby"},
                      Me:look()) end),
     ?It("should move to the kitchen", fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         Me:move("north"),
         ?assertEqual({ok, "It's a kitchen"},
                      Me:look()) end),
  ?Describe("Rooms With Items",
    [?It("should show items in the lobby",fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony","Hello"),
         room:add_to_room(lobby, "a wrench"),
         ?assertEqual({ok, "It's a lobby\n\ta wrench"},
                      Me:look()) end),
     ?It("should take an item from the lobby", fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         room:add_to_room(lobby, "A wombat"),
         Me:take("A wombat"),
         ?assertEqual({ok, "You have\n\tA wombat"},
                      Me:inventory()),
         ?assertEqual({ok, "It's a lobby"},
                      Me:look()) end),
     ?It("shouldn't take an item that isn't there", fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         ?assertEqual({not_found, "my wallet"},
                      Me:take("my wallet")) end),
     ?It("shouldn't drop an item I don't have", fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         ?assertEqual({not_found, "my wallet"},
                      Me:drop("my wallet")),
         ?assertEqual({ok, "It's a lobby"},
                      Me:look()) end),
     ?It("should drop an item in the kitchen", fun setup/0, fun cleanup/1,
         begin
         Me = player:login("Tony", "Hello"),
         room:add_to_room(lobby, "my wallet"),
         Me:take("my wallet"),
         Me:move("north"),
         Me:drop("my wallet"),
         ?assertEqual({ok,"You aint got jack!"},
                      Me:inventory()),
         ?assertEqual({ok,"It's a kitchen\n\tmy wallet"},
                      Me:look()) end)
      ])
   ]),
  ?Describe("Communications",
    [?It("should list the players registered", fun setup/0, fun cleanup/1,
         begin
         registry:add_player("Liv", kitchen, self),
         Me = player:login("Tony", "Hello"),
         ?assertEqual({ok, "The players logged in are\nTony : lobby\nLiv : kitchen"},
                      Me:who()) end)
  ])
].

player_database_test_() -> [
  ?Describe("load from the database",
    [?It("should load a player from the database",
         fun insert_greg/0, fun delete_greg/1,
         begin
         Me = player:login("Greg", "thing"),
         ?assertEqual({ok, "It's a lobby"}, Me:look())
         end),
     ?It("should fail when the password isn't in the database",
         fun insert_greg/0, fun delete_greg/1,
         begin
         Me = player:login("Greg", "Monkey!"),
         ?assertEqual(error, Me)
         end),
     ?It("should locate the player in the correct room",
         fun insert_greg/0, fun delete_greg/1,
         begin
         Me = player:login("Greg", "thing"),
         ?assertEqual({ok, "It's a lobby"}, Me:look())
         end)
    ]),
  ?Describe("update changes when they happen",
    [?It("should remember a picked up item",
         fun insert_greg/0, fun delete_greg/1,
         begin
         Me = player:login("Greg", "thing"),
         room:add_to_room(lobby, "a wrench"),
         Me:take("a wrench"),
         {atomic, Rows} = mnesia:transaction( fun() ->
                     qlc:eval( qlc:q([X || X <- mnesia:table(player)])) end),
         [Greg] = lists:filter((fun(X) -> "Greg" =:= X#player.name end), Rows),
         ?assertEqual(["a wrench", "Book"], Greg#player.items)
         end),
     ?It("should remember a dropped item",
         fun insert_greg/0, fun delete_greg/1,
         begin
         Me = player:login("Greg", "thing"),
         Me:drop("Book"),
         {atomic, Rows} = mnesia:transaction(fun() ->
                     qlc:eval( qlc:q([X || X<-mnesia:table(player)])) end),
         [Greg] = lists:filter((fun(X) -> "Greg" =:= X#player.name end), Rows),
         ?assertEqual([], Greg#player.items)
         end)
    ])     
].
insert_greg() ->
        setup(),
        mnesia:transaction(fun() ->
                mnesia:write(
                        #player{key="Greg",
                                password="thing",
                                name="Greg",
                                description="Just this guy",
                                location_key=kitchen,
                                items=["Book"]}
                )
        end).
% starting and stopping greg is your problem, not the deletion's
delete_greg(Pid) ->
        cleanup(Pid),
        mnesia:transaction(fun() -> 
                                mnesia:delete(player, "Greg") end),
        true.
setup() ->
        % I don't know why, but I need the print
        % to make the kitchen test pass
        io:format(""),
         registry:start(),
        stubs:fake_rooms(),
        mnesia:transaction(fun() ->
                mnesia:write(
                        #player{key="Tony",
                                password="Hello",
                                name="Tony",
                                description="Stuff",
                                location_key=kitchen,
                                items=[]}
                )
        end).
cleanup(_Pid) ->
        stubs:stop_fake_rooms(),
        gen_server:cast({global,registry}, stop),
        true.
