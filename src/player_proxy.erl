-module(player_proxy,[Pid]).
-export([look/0,look/1,move/1,take/1,drop/1,inventory/0,command/2,exit/0]).

look() ->
        gen_server:call(Pid, look).
look(Item) ->
        gen_server:call(Pid, {look, Item}).
move(Direction) ->
        gen_server:call(Pid, {move, Direction}).
take(Item) ->
        Pid ! {take,self,Item}.
drop(Item) ->
        Pid ! {drop,self,Item}.
inventory() ->
        gen_server:call(Pid, inventory).
command(Command,Args) ->
        Pid ! {command, self, Command, Args}.
exit() ->
        Pid ! {exit, self}.
