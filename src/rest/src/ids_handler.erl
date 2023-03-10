-module(ids_handler).
-behavior(cowboy_handler).


-export([init/2]).

init(Req=#{method := <<"GET">>}, State) ->
    logging:log("Received GET request in ids_handler"),
    Table = delivery,
    {Status, Result} = mnesia_wrapper:transaction(last_id, Table, none),


    Response = case Status of
                    atomic ->
                        cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>
                        }, jiffy:encode(#{
                                info => success,
                                result => Result    
                            }), Req);
                    aborted ->
                        cowboy_req:reply(400, #{
                            <<"content-type">> => <<"application/json">>
                        }, "", Req)
            end,

    {ok, Response, State}.