-module(db_initializer).
-export([init/0]).
%-export([init/0, listen/0]) .
-export([init/0,addRange/1,addRangeWrapper/1,add/1,select/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").


init() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(delivery,
        [{attributes, record_info(fields, delivery)}, 
        {disc_copies, [node()]}]). 


%init_listener() -> 
%    register(db_listener, spawn(db_initializer, listen, [])).
%
%listen() ->
%    receive
%        {exec_local, From, Fun} -> {Status, Result} = exec(Fun),
%                                From ! {result, Status, Result};
%        {exec_remote, {FromPid, _}, Fun} -> 
%            {Status, Result} = exec(Fun),
%                                FromPid ! {result, Status, Result};
%        _ ->  ok
%    end,
%    listen().

%exec(Fun) -> 
%    mnesia:transaction(Fun).

% addRange(List) ->
%    if length(List) > 0 -> 
%        Fun = fun() ->
%            addRangeWrapper(List)
%    	end,
%        mnesia:transaction(Fun);
%    true ->
%        ok
%    end.    


%addRangeWrapper( [ First | Rest ] ) ->
%    mnesia:write( First ),
%    addRangeWrapper( Rest );
%addRangeWrapper( [ ] ) ->
%    ok.

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
