-module(registry_tests).
-include("tests.hrl").

registry_test_() -> [
  ?Describe("Adding and removing from the registry",
    [?It("should add a player to the room", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self) ,
         ?assertEqual({ok, [{"Tony", lobby, self}]},
                      registry:players()) end),
     ?It("should remove a player from a room", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self),
         ok = registry:remove_player("Tony"),
         ?assertEqual({ok, []},
                      registry:players()) end),
     ?It("should remove a player's old location if adding to a new room", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self),
         ok = registry:add_player("Tony", kitchen, self),
         ?assertEqual({ok, [{"Tony", kitchen, self}]},
                      registry:players()) end),
     ?It("should handle multiple players", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self),
         ok = registry:add_player("Liv", lobby, self),
         ok = registry:add_player("Tony", kitchen, self),
         ?assertEqual({ok, [{"Tony", kitchen, self},
                            {"Liv", lobby, self}]},
                      registry:players()) end)
  ]),
  ?Describe("Interacting with rooms",
    [?It("should show only people in a room", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self),
         ok = registry:add_player("Liv", lobby, self),
         ok = registry:add_player("Marco", kitchen, self),
         ?assertEqual({ok, [{"Marco", kitchen, self}]},
                      registry:players_in_room(kitchen)) end)
  ])
].

setup()-> io:format(""), {ok,Pid} = registry:start(), Pid.
cleanup(Pid) -> gen_server:cast(Pid, stop).
