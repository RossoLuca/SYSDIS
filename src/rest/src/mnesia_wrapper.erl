-module(mnesia_wrapper).
-include("records.hrl").
-export([transaction/1]).

transaction(Fun) ->
    {db_listener, 'db@db_host'} ! {remote_transaction, {node(), self()}, Fun},
    receive
        {result, Status, Result} -> {Status, Result}
    end.

