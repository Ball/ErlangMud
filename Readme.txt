
Some useage concepts...
* player_proxy() = player:login(UserName, Password)
* player_proxy:look().
* player_proxy:move("north").
player_proxy:say("Boo!").
player_proxy:talk("Hello").
player_proxy:who().
player_proxy:help().

the player starts in the "Lobby"

There is a room locator
There is a player locator
        locators update information based on events?

=> script...
>room:start_room("Lobby", "You are in the lobby").
ok
>Me = player:login("Tony", "Hello").
{player_proxy,<0.73.0>}
>Me:look().
"You are in the lobby~n"
