-module(player_tests).
-include("tests.hrl").

player_test_() ->
  ?Describe("Bad Password",
    [?It("Should return an error",
          fun setup/0,
          fun cleanup/1,
          ?_test(begin ?assertEqual(error, player:login("Tony", "BassPassword"))end))
    ]),
  ?Describe("Good Password",
    [?It("Should have a player proxy", fun setup/0, fun cleanup/1,
         ?_test(begin ?assertEqual({ok,_Pid}, player:login("Tony", "Hello"))end)),
     ]).

setup() ->
  player:start_player(player_one).
cleanup(_Pid) ->
        player:stop_player(player_one),
        true.
