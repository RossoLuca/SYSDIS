-module(drone_kill_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"GET">>}, State ) ->
    ParsedQs = cowboy_req:parse_qs(Req0),
    io:format("~n~p", [ParsedQs]),
    AtomQs = [{binary_to_atom(K), binary_to_list(V)} || {K, V} <- ParsedQs],
    case lists:search(fun({Key, _Value}) -> 
                            case Key of
                                id -> true;
                                _ -> false
                            end
                        end, AtomQs) of
        {value, {id, Id}} -> 
            drone_hub_wrapper:notify(kill,Id),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, "", Req0),
            {ok, Req, State};
        false -> 
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, "", Req0),
            {ok, Req, State}
    end;

    
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req, State}.