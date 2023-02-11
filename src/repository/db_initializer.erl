-module(db_initializer).
-export([init/0]) .
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").


init() ->
    mnesia:start().
    mnesia:create_schema([node()]).
    mnesia:create_table(delivery,
                        [{attributes, record_info(fields, delivery)}]).

addRange(List) ->
    if length(List) > 0 -> 
        Fun = fun() ->
            addRangeWrapper(List)
    	end,
        mnesia:transaction(Fun);
    true ->
        ok
    end.    


addRangeWrapper( [ First | Rest ] ) ->
    mnesia:write( First ),
    addRangeWrapper( Rest );
addRangeWrapper( [ ] ) ->
    ok.