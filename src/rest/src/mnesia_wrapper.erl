-module(mnesia_wrapper).
-include("records.hrl").
-export([transaction/1]).

transaction(Fun) ->
    {db_listener, 'db@127.0.0.1'} ! {remote_transaction, {node(), self()}, Fun},
    receive
        {result, Status, Result} -> {Status, Result}
    end.

