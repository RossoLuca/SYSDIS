-module(db_initializer).
-export([init/0,addRange/1,addRangeWrapper/1,add/1,select/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").


init() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
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


add( Item ) ->
    Item.


select( Entity, {MatchHead, Guard, Result} ) ->
    {Entity,MatchHead,Guard,Result}.


% get_all( Entity ) -> 
%     select( Entity, '_', [ ], ['$_'] ).


% counter( Entity ) ->
%     length( get_all( Entity ) ).