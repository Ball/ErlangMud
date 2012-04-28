-module(player_proxy,[Pid]).
-export([look/0,look/1,move/1,take/1,drop/1,inventory/0,who/0,shout/1,tell/2,whisper/2,say/1,command/2,exit/0,create_room/3, create_exit/4,destroy_room/1]).

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
tell(Nic, Message) ->
        gen_server:call(Pid, {tell, Nic, Message}).
whisper(Nic, Message) ->
        gen_server:call(Pid, {whisper, Nic, Message}).
shout(Message) ->
        gen_server:call(Pid, {shout, Message}).
say(Message) ->
        gen_server:call(Pid, {say, Message}).
command(Command,Args) ->
        Pid ! {command, self, Command, Args}.
create_room(Key,Name,Description) ->
        room:create_room(Key, Name, Description).
create_exit(FromRoom, ToRoom, Direction, Description) ->
	room:create_exit(FromRoom, ToRoom, Direction, Description).
destroy_room(Key) ->
        room:destroy_room(Key).
exit() ->
        Pid ! {exit, self}.
