-module(http_utils).

%-define(ENDPOINT, "localhost").
-define(ENDPOINT, "rest_host").

-export([createConnection/0, doGet/2, doPost/3]).

createConnection() -> 
    case gun:open(?ENDPOINT, 8080) of
        {ok, Connection} -> 
            Connection;
        {error, timeout} ->
            connection_timed_out
    end.

doGet(Connection, Path) -> 
    StreamRef = gun:get(Connection, Path, [
        {<<"accept">>, "application/json"}
    ]),
    case gun:await(Connection, StreamRef) of
        {response, fin, _Status, _Headers} ->
            no_data;
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(Connection, StreamRef),
            jiffy:decode(Body, [return_maps])
    end.


doPost(Connection, Path, Data) ->
    Body = jiffy:encode(Data),
    StreamRef = gun:post(Connection, Path, [
        {<<"content-type">>, "application/json"}
    ], Body),
    case gun:await(Connection, StreamRef) of
        {response, fin, _Status, _Header} ->
            no_data;
        {response, nofin, _Status, _Headers} ->
            {ok, ResponseBody} = gun:await_body(Connection, StreamRef),
            jiffy:decode(ResponseBody)
    end.