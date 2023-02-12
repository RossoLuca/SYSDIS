-module(active_drone_handler).
-behavior(cowboy_handler).
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"GET">>}, State ) ->
    MatchHead = #delivery{stato='$1', _='_'},
    GuardPending = {'=', '$1',"pending"},
    GuardFlying = {'=', '$1',"flying"},
    Result = ['$_'],
    {atomic,Data} = db_initializer:select(delivery,[{MatchHead, [GuardPending,GuardFlying],[Result]}]),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Data), Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req, State}.