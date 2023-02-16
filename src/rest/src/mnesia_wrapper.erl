-module(mnesia_wrapper).
-include("records.hrl").
-export([transaction/3]).

transaction(write, Table, Data) ->
    % DbNode = os:getenv{"DB_HOST","error"},
    {db_listener, 'db@db_host'} ! {write, {node(), self()}, Table, Data},
    handle_response();

transaction(select, Table, Spec) ->
    {db_listener, 'db@db_host'} ! {select, {node(), self()}, Table, Spec},
    handle_response();

transaction(read_by_id, Table, Id) ->
    {db_listener, 'db@db_host'} ! {read_by_id, {node(), self()}, Table, Id},
    handle_response().    


handle_response() ->
    receive
        {result, Status, Result} -> 
            %io:format("~n ~p ~n ~p", [Status, Result]),
            {Status, Result}
    after 10000 ->
        #{result => aborted, info => 'db_connection_error'}
    end.