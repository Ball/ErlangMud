-module(registry_tests).
-include("tests.hrl").

registry_test_() -> [
  ?Describe("Adding and removing from the registry",
    [?It("should add a player to the room", fun setup/0, fun cleanup/1,
         begin
         ok = registry:add_player("Tony", lobby, self) ,
         ?assertEqual({ok, [{"Tony", lobby, self}]},
                      registry:players()) end)
  ])
].

setup()-> io:format(""), {ok,Pid} = registry:start(), Pid.
cleanup(Pid) -> gen_server:cast(Pid, stop).
