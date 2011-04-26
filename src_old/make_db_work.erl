-module(make_db_work).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("em_records.herl").

init() ->
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(
                player,
                [{disc_copies, [node()]},
                 {attributes, record_info(fields, player)}]),
        mnesia:create_table(
                room,
                [{disc_copies, [node()]},
                 {attributes, record_info(fields, room)}]).

