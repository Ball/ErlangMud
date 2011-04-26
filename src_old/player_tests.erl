-module(player_tests).
-include("tests.hrl").
-export([module_setup/0]).

player_test_() ->[
        ?Describe("After connecting",
                [?It("Should have a location",
                     fun setup/0,
                     fun cleanup/1,
                     ?_test(begin ?assertEqual(room_kitchen,
                                               player:location(player_one)) end)),
                 ?It("Should have a room know about the player",
                     fun setup/0,
                     fun cleanup/1,
                     ?_test(begin ?assertEqual([player_one],
                                               room:occupants(room_kitchen)) end))
                ]),
        ?Describe("Describing a room",
                [?It("Should match the room's description",?_fail()),
                 ?It("Should have a list of exits",?_fail())
                ]),
        ?Describe("Traveling to another room",
                [?It("Should be in the second room", ?_fail()),
                 ?It("Should be known by the second room", ?_fail()),
                 ?It("Should not be known by the first room", ?_fail())])
        ].
module_setup()->
  case player:does_exist(player_one) of
    false ->
      player:create_player(player_one, "One", "A tall, skinny number")
  end.
setup() ->
        player:start_player(player_one).
cleanup(_Pid)->
        player:stop_player(player_one),
        true.
