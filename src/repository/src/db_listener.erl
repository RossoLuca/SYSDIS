-module(db_listener).
-include("records.hrl").
-export([start_link/1, loop/0, execute/1]).

start_link(_Name) -> 
    Pid = spawn_link(?MODULE, loop, []),
    DbProcess = list_to_atom(os:getenv("DB_PROCESS")),  
    register(DbProcess, Pid),
    {ok, Pid}.


loop() -> 
    receive
        {write, {_FromNode, FromPid}, Table, Data} ->
            case check_table_exists(Table) of
                true ->
                    io:format("Received write action from ~p on table ~p of ~p~n", [FromPid, Table, Data]),
                    % To be added check that Data match Table's attributes
                    WriteFun = fun() -> 
                        Id = Data#delivery.id,
                        Result = mnesia:read({Table, Id}),
                        if length(Result) > 0 ->
                            mnesia:write(Data);
                        true -> 
                            mnesia:dirty_update_counter(table_ids, Table, 1),
                            mnesia:write(Data)
                        end
                    end,
                    {Status, Result} = execute(WriteFun),
                    FromPid ! {result, Status, Result};
                false ->
                    io:format("Received write action from ~p on table ~p that is not defined~n", [FromPid, Table]),
                    {Status, Reason} = {aborted, table_not_exists},
                    FromPid ! {result, Status, Reason}
            end,
            loop();
        {select, {_FromNode, FromPid}, Table, Specification} ->
            case check_table_exists(Table) of
                true -> 
                    io:format("Received select action from ~p on table ~p of ~p~n", [FromPid, Table, Specification]),
                    SelectFun = fun() -> mnesia:select(Table, Specification) end,
                    {Status, Result} = execute(SelectFun),
                    io:format("~n ~p ~n ~p", [Status, Result]),
                    if Status == aborted ->
                            FromPid ! {result, Status, Result};
                        true ->
                            List = lists:map(fun(T) -> lists:nth(1, T) end, Result),
                            AdaptedResult = adaptResponse(List),
                            FromPid ! {result, Status, AdaptedResult}
                    end;
                false ->
                    io:format("Received select action from ~p on table ~p that is not defined~n", [FromPid, Table]),
                    {Status, Reason} = {aborted, table_not_exists},
                    FromPid ! {result, Status, Reason}
                end,
            loop();
        {read_by_id, {_FromNode, FromPid}, Table, Id} ->
            case check_table_exists(Table) of
                true ->
                    io:format("Received read by id action from ~p on table ~p of ~p~n", [FromPid, Table, Id]),
                    ReadFun = fun() -> mnesia:read({Table, Id}) end,
                    {Status, Result} = execute(ReadFun),
                    if Status == aborted ->
                            FromPid ! {result, Status, Result};
                        true ->
                            AdaptedResult = adaptResponse(Result),
                            FromPid ! {result, Status, AdaptedResult}
                    end;
                false ->
                    io:format("Received read by id action from ~p on table ~p that is not defined~n", [FromPid, Table]),
                    {Status, Reason} = {aborted, table_not_exists},
                    FromPid ! {result, Status, Reason}
            end,
            loop();
        {last_id, {_FromNode, FromPid}, Table, _Data} ->
            case check_table_exists(Table) of
                true ->
                    io:format("Received last id action from ~p on table ~p~n", [FromPid, Table]),
                    Id = mnesia:dirty_update_counter(table_ids, Table, 0),
                    {Status, Result} = {atomic, Id},
                    FromPid ! {result, Status, Result};
                false ->
                    io:format("Received last id action from ~p on table ~p that is not defined~n", [FromPid, Table]),
                    {Status, Reason} = {aborted, table_not_exists},
                    FromPid ! {result, Status, Reason}
            end,
            loop();
        {_Operation, {_FromNode, FromPid}, _Table, _Specification} -> 
            Status = 'aborted',
            Result = 'undefined_operation',
            FromPid ! {result, Status, Result},
            loop();
        _ -> 
            loop()
    end.


execute(Transaction) ->
    mnesia:transaction(Transaction).

adaptResponse(Deliveries) -> 
    lists:map(fun(Del) -> 
                #{id => Del#delivery.id,
                    pid => Del#delivery.pid,
                    state => Del#delivery.state,
                    start_x => Del#delivery.start_x,
                    start_y => Del#delivery.start_y,
                    current_x => Del#delivery.current_x,
                    current_y => Del#delivery.current_y,
                    end_x => Del#delivery.end_x,
                    end_y => Del#delivery.end_y,
                    fallen => Del#delivery.fallen
                }  
    end, Deliveries).


check_table_exists(Table) ->
    Tables = mnesia:system_info(tables),
    case lists:search(fun(T) ->
                        if Table == T ->
                            true;
                        true ->
                            false
                        end
                end, Tables) of
        {value, Table} -> true;
        false -> false
    end.