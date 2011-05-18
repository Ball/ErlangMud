-module(player_tests).
-include("tests.hrl").

player_test_() ->[
  ?Describe("Bad Password",
    [?It("should return an error",fun setup/0,fun cleanup/1,
          ?_test(begin ?assertEqual(error, player:login("Tony", "BassPassword"))end))
    ]),
  ?Describe("Good Password",
    [?It("should have a player proxy",fun setup/0,fun cleanup/1,
                    ?_test(begin Me = player:login("Tony", "Hello"),
                                 ?assertEqual({ok,"You aint got jack!"},
                                 Me:inventory())
                           end))
     ]),
  ?Describe("Room Interaction",
    [?It("should describe the lobby",fun setup/0, fun cleanup/1,
         ?_test(begin Me = player:login("Tony", "Hello"),
                      ?assertEqual({ok, "It's a lobby"},
                                   Me:look())
                end)),
     ?It("should move to the kitchen", fun setup/0, fun cleanup/1,
         ?_test(begin Me = player:login("Tony", "Hello"),
                      Me:move("north"),
                      ?assertEqual({ok, "It's a kitchen"},
                                   Me:look()) end)),
    ?Describe("Rooms With Items",
      [?It("should show items in the lobby",fun setup/0, fun cleanup/1,
           ?_test(begin Me = player:login("Tony","Hello"),
                        gen_server:call(lobby, {add_item,"a wrench"}),
                        ?assertEqual({ok, "It's a lobby\n\ta wrench"},
                                     Me:look()) end))
      ])
   ])
].

setup() ->
        % I don't know why, but I need the print
        % to make the kitchen test pass
        io:format(""),
        stubs:fake_rooms().
cleanup(_Pid) ->
        stubs:stop_fake_rooms(),
        true.
