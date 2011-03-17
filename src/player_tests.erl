-module(player_tests).
-include("tests.hrl").

room_test_() ->[
        ?Describe("After connecting",
                [?It("Should have a location", ?_fail()),
                 ?It("Should have a room know about the player",?_fail())
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
