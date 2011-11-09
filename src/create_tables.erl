-module(create_tables).
-compile(export_all).
-include("records.hrl").
drop_tables() ->
  mnesia:delete_table(player),
  mnesia:delete_table(room).
init_tables() ->
        mnesia:create_table(player, [{disc_copies, []}, {attributes, record_info(fields, player)}]),
        mnesia:create_table(room,   [{disc_copies, []}, {attributes, record_info(fields, room)}]).
