-module(stubs).
-include("src/records.hrl").
-export([fake_rooms/0,login_with_rooms/0]).

fake_rooms() ->
        room:start_room(lobby, "Lobby", "It's lobby", [#room_exit{direction="north", description="an archway", location_key=kitchen}]),
        room:start_room(kitchen, "Kitchen", "It's a kitchen", [#room_exit{direction="south", description="dutch doors", location_key=lobby}]).
login_with_rooms() ->
        fake_rooms(),
        player:login("Tony","Hello").
        
