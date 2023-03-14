-module(http_utils).

-export([createConnection/0, createConnection/1, doGet/2, doPost/3]).
 
% Returns an HTTP connection to the Rest API
createConnection() ->
    Endpoint = os:getenv("REST_ENDPOINT", undefined),
    if Endpoint == undefined ->
        logging:log("Rest endpoint is not defined. Please define it inside the Dockerfile."),
        exit(self(), kill);
    true ->
        ok
    end,
    case gun:open(Endpoint, 8080) of
        {ok, Connection} -> 
            Connection;
        {error, timeout} ->
            connection_timed_out
    end.

% Returns an HTTP connection to the Rest API on the endpoint Endpoint
createConnection(Endpoint) ->
    case gun:open(Endpoint, 8080) of
        {ok, Connection} -> 
            Connection;
        {error, timeout} ->
            connection_timed_out
    end.
    
% Using Connection, make an HTTP GET request to the Rest API on the path Path
doGet(Connection, Path) -> 
    StreamRef = gun:get(Connection, Path, [
        {<<"accept">>, "application/json"}
    ]),
    case gun:await(Connection, StreamRef) of
        {response, fin, _Status, _Headers} ->
            no_data;
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(Connection, StreamRef),
            jiffy:decode(Body, [return_maps]);
        {error, timeout} ->
            logging:log("The Rest API isn't reachable in this moment. Process will be restarted to do another attempt"),
            exit(self(), kill)
    end.

% Using Connection, make an HTTP POST request to the Rest API on the path Path including Data inside the body
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
            jiffy:decode(ResponseBody);
        {error, _} ->
            connection_closed
    end.