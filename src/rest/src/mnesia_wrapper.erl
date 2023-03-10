-module(mnesia_wrapper).
-include("records.hrl").
-export([transaction/3]).

transaction(write, Table, Data) ->
    Process = get_db_process(),
    Process ! {write, {node(), self()}, Table, Data},
    handle_response();

transaction(select, Table, Spec) ->
    Process = get_db_process(),
    Process ! {select, {node(), self()}, Table, Spec},
    handle_response();

transaction(read_by_id, Table, Id) ->
    Process = get_db_process(),
    Process ! {read_by_id, {node(), self()}, Table, Id},
    handle_response();    

transaction(last_id, Table, _Data) -> 
    Process = get_db_process(),
    Process ! {last_id, {node(), self()}, Table, _Data},
    handle_response().

handle_response() ->
    receive
        {result, Status, Result} -> 
            %io:format("~n ~p ~n ~p", [Status, Result]),
            {Status, Result}
    after 10000 ->
        #{result => aborted, info => 'db_connection_error'}
    end.

get_db_process() ->
    Process = list_to_atom(os:getenv("DB_PROCESS")),
    Host = list_to_atom(os:getenv("DB_HOST")),
    {Process, Host}.