-module(active_drone_handler).
-behavior(cowboy_handler).
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"GET">>}, State ) ->
    MatchHead = #delivery{stato='$1', _='_'},
    GuardPending = {'=', '$1',"pending"},
    GuardFlying = {'=', '$1',"flying"},
    Result = ['$_'],
    Fun = fun() ->
        mnesia:select(delivery,[{MatchHead, [GuardPending,GuardFlying],[Result]}])
    end,
    {Status, Result} = mnesia_wrapper:transaction(Fun),
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