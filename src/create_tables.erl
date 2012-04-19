-module(create_tables).
-compile(export_all).
-include("records.hrl").

create_schema() ->
  mnesia:create_schema([node()]).
delete_schema() ->
  mnesia:delete_schema([node()]).

drop_tables() ->
  mnesia:delete_table(player),
  mnesia:delete_table(room).

init_tables() ->
    mnesia:create_table(player, [{disc_copies, [node()]}, {attributes, record_info(fields, player)}]),
    mnesia:create_table(room,   [{disc_copies, [node()]}, {attributes, record_info(fields, room)}]).
