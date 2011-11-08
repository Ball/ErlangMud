-module(player_tests).
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

setup() ->
        % I don't know why, but I need the print
        % to make the kitchen test pass
        io:format(""),
         registry:start(),
        stubs:fake_rooms().
cleanup(_Pid) ->
        stubs:stop_fake_rooms(),
        gen_server:cast({global,registry}, stop),
        true.
