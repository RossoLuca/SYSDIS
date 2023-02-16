-module(db_listener).
-include("records.hrl").
-export([start_link/1, loop/0, execute/1]).

start_link(_Name) -> 
    Pid = spawn_link(?MODULE, loop, []),
    register(db_listener, Pid),
    {ok, Pid}.


loop() -> 
    receive
        {write, {_FromNode, FromPid}, Table, Data} ->
            % To replace with a log util
            io:format("Received write action from ~p on table ~p of ~p~n", [FromPid, Table, Data]),
            % To be added check that Table is defined on db and that Data match Table's attriutes
            WriteFun = fun() -> mnesia:write(Data) end,
            {Status, Result} = execute(WriteFun),
            FromPid ! {result, Status, Result},
            loop();
        {select, {_FromNode, FromPid}, Table, Specification} -> 
            io:format("Received select action from ~p on table ~p of ~p~n", [FromPid, Table, Specification]),
            % To be added check that Table is defined on db
            SelectFun = fun() -> mnesia:select(Table, Specification) end,
            {Status, Result} = execute(SelectFun),
            io:format("~n ~p ~n ~p", [Status, Result]),
            if Status == aborted ->
                    FromPid ! {result, Status, Result};
                true ->
                    List = lists:map(fun(T) -> lists:nth(1, T) end, Result),
                    AdaptedResult = adaptResponse(List),
                    FromPid ! {result, Status, AdaptedResult}
            end,
            loop();
        {read_by_id, {_FromNode, FromPid}, Table, Id} ->
            io:format("Received read by id action from ~p on table ~p of ~p~n", [FromPid, Table, Id]),
            % To be added check that Table is defined on db
            ReadFun = fun() -> mnesia:read({Table, Id}) end,
            {Status, Result} = execute(ReadFun),
            if Status == aborted ->
                    FromPid ! {result, Status, Result};
                true ->
                    AdaptedResult = adaptResponse(Result),
                    FromPid ! {result, Status, AdaptedResult}
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
                    stato => Del#delivery.stato,
                    start_x => Del#delivery.start_x,
                    start_y => Del#delivery.start_y,
                    current_x => Del#delivery.current_x,
                    current_y => Del#delivery.current_y,
                    end_x => Del#delivery.end_x,
                    end_Y => Del#delivery.end_y
                }  
    end, Deliveries).