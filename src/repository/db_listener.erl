-module(db_listener).
-export([start_link/1, loop/0, execute/1]).

start_link(_Name) -> 
    Pid = spawn_link(?MODULE, loop, []),
    register(db_listener, Pid),
    {ok, Pid}.

loop() ->
    receive
        {remote_transaction, {_FromNode, FromPid}, Transaction} ->
            {Status, Result} = execute(Transaction),
            FromPid ! {result, Status, Result},
            loop();
        _ -> exit(self(), error)
    end.


execute(Transaction) ->
    mnesia:transaction(Transaction).