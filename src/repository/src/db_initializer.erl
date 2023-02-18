-module(db_initializer).
-export([init/0]).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").


init() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(delivery,
                        [{attributes, record_info(fields, delivery)}]).