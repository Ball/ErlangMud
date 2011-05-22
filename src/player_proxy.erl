-module(player_proxy,[Pid]).
-export([look/0,look/1,move/1,take/1,drop/1,inventory/0,who/0,say/1,command/2,exit/0]).

look() ->
        gen_server:call(Pid, look).
look(Item) ->
        gen_server:call(Pid, {look, Item}).
move(Direction) ->
        gen_server:call(Pid, {move, Direction}).
take(Item) ->
        gen_server:call(Pid, {take, Item}).
drop(Item) ->
        gen_server:call(Pid, {drop, Item}).
inventory() ->
        gen_server:call(Pid, inventory).
who() ->
        gen_server:call(Pid, who).
say(Message) ->
        gen_server:call(Pid, {say, Message}).
command(Command,Args) ->
        Pid ! {command, self, Command, Args}.
exit() ->
        Pid ! {exit, self}.
