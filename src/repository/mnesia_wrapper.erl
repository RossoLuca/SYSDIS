-module(mnesia_wrapper).
-include("records.hrl").
-export([transaction/1, createDel/1]).

transaction(Fun) ->
    {db_listener, 'db@db_host'} ! {remote_transaction, {node(), self()}, Fun},
    receive
        {result, Result, Status} -> {Result, Status}
    end.

createDel(Id) -> 
    #delivery{
        id=Id,
        pid=0,
        stato=ground,
        start_x=0,
        start_y=0,
        current_x=0,
        current_y=0,
        end_x=1,
        end_y=1}.

