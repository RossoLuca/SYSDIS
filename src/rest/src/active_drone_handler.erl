-module(active_drone_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"GET">>}, State ) ->
    MatchHead = #delivery{stato='$1', _='_'},
    GuardActive = {'=/=', '$1',completed},
    MatchResult = ['$_'],

    MatchSpecs = [{MatchHead, [GuardActive],[MatchResult]}],
    
    io:format("~p~n", [MatchSpecs]),
    {Status, Result} = mnesia_wrapper:transaction(select, delivery, MatchSpecs),
    Req = return_req(Status,Result,Req0),
    {ok, Req, State};
    
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req, State}.

return_req(atomic,Result,Req0)->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Result), Req0);
return_req(aborted,_,Req0)->
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, "", Req0).