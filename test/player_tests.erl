-module(player_tests).
-include("tests.hrl").

player_test_() ->[
  ?Describe("Bad Password",
    [?It("Should return an error",
          ?_test(begin ?assertEqual(error, player:login("Tony", "BassPassword"))end))
    ]),
  ?Describe("Good Password",
    [?It("Should have a player proxy",
                    ?_test(begin ?assertEqual({ok,"You aint got jack!"},
                                              (player:login("Tony", "Hello")):inventory())
                           end))
     ])
].

setup() ->
  player:start_player(player_one).
cleanup(_Pid) ->
        player:stop_player(player_one),
        true.
