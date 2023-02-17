-module(delivery_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).


-export([init/2]).

init( Req0=#{method := <<"POST">>}, State0 ) ->
	{ok, Data, Req1} = cowboy_req:read_body(Req0),
    
    DecodedTuple = jiffy:decode( Data ),
	{ [	{ <<"id">>, Id },
		{ <<"pid">>, Pid },
		{ <<"stato">>, Stato },
		{ <<"start_x">>, Start_x },
		{ <<"start_y">>, Start_y },
		{ <<"current_x">>, Current_x },
		{ <<"current_y">>, Current_y },
		{ <<"end_x">>, End_x },
		{ <<"end_y">>, End_y }
    ] } = DecodedTuple, 

    io:format("~p ~n", [DecodedTuple]),
    Delivery = #delivery{
        id=Id,
        pid = list_to_atom(binary_to_list(Pid)),
        stato = list_to_atom(binary_to_list(Stato)),
        start_x = Start_x,
        start_y = Start_y,
        current_x = Current_x,
        current_y = Current_y,
        end_x = End_x,
        end_y = End_y
    },

    erlang:display(Delivery),

    {Status, Result} = mnesia_wrapper:transaction(write, delivery, Delivery),
    io:format("~p ~n", [Status]),
    io:format("~p ~n", [Result]),
    Req = return_req(Status,Result,Req1),
    {ok, Req, State0};

init( Req0=#{method := <<"GET">>}, State0 ) ->
    ParsedQs = cowboy_req:parse_qs(Req0),
    io:format("~n~p", [ParsedQs]),
    AtomQs = [{binary_to_atom(K), binary_to_list(V)} || {K, V} <- ParsedQs],
    io:format("~n~p", [AtomQs]),
    
    case lists:search(fun({Key, _Value}) -> 
                            case Key of
                                id -> true;
                                _ -> false
                            end
                        end, AtomQs) of
        {value, {id, Id}} -> 
            read_by_id(Id, Req0, State0);
        false -> 
            %% da implementare quando non trova id nella query string
            %% select(AtomQs, Req0, State0)
            return_req(aborted,#{reason => "not implemented"},Req0)
    end;

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST,GET">>
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

read_by_id(Id, Req, State) ->
    {Status, Result} = mnesia_wrapper:transaction(read_by_id, delivery, list_to_integer(Id)),
    io:format("~n~p~n~p", [Status, Result]),
    Response = return_req(Status,Result,Req),
    {ok, Response, State}.

% select(AtomQs, Req, State) -> 
%     ok.