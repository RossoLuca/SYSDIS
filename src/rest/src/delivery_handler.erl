-module(delivery_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).


-export([init/2]).
-define(MAXSIZE,1000.0).

init( Req0=#{method := <<"POST">>}, State0 ) ->
	{ok, Data, Req1} = cowboy_req:read_body(Req0),
    DecodedTuple = jiffy:decode( Data , [return_maps]),

    Id = maps:get(<<"id">>,DecodedTuple),
    Pid = maps:get(<<"pid">>,DecodedTuple),
    State = maps:get(<<"state">>,DecodedTuple),
    Start_x = maps:get(<<"start_x">>,DecodedTuple),
    Start_y = maps:get(<<"start_y">>,DecodedTuple),
    Current_x = maps:get(<<"current_x">>,DecodedTuple),
    Current_y = maps:get(<<"current_y">>,DecodedTuple),
    End_x = maps:get(<<"end_x">>,DecodedTuple),
    End_y = maps:get(<<"end_y">>,DecodedTuple),
    Fallen = maps:get(<<"fallen">>,DecodedTuple),
    
    Delivery = #delivery{
        id=Id,
        pid = binary_to_atom(Pid),
        state = binary_to_atom(State),
        start_x = Start_x,
        start_y = Start_y,
        current_x = Current_x,
        current_y = Current_y,
        end_x = End_x,
        end_y = End_y,
        fallen = Fallen
    },

    erlang:display(Delivery),
    case delivery_check(Delivery) of
        {ok,_} ->
            {Status, Result} = mnesia_wrapper:transaction(write, delivery, Delivery),
            Req = return_req(Status,Result,Req1),
            {ok, Req, State0};
        {Errore,false} ->
            Req = return_req(aborted,#{reason => Errore},Req1),
            {ok, Req, State0}
    end;

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

delivery_check(Del) ->
    Id = Del#delivery.id,
    Start_x = Del#delivery.start_x,
    End_x = Del#delivery.end_x,
    Start_y = Del#delivery.start_y,
    End_y = Del#delivery.end_y,
    case {Id,Start_x,End_x,Start_y,End_y} of
        {Id,_,_,_,_} when Id =< 0 ->
            {iderror,false};
        {_,Start_x,End_x,Start_y,End_y} when Start_x < 0; Start_y < 0;End_x < 0 ; End_y < 0 ->
            {coordinateserror,false};
        {_,Start_x,End_x,Start_y,End_y} when Start_x > ?MAXSIZE; Start_y > ?MAXSIZE;End_x > ?MAXSIZE ; End_y > ?MAXSIZE ->
            {coordinateserror,false};
        {_,Start_x,End_x,Start_y,End_y} when Start_x == End_x , Start_y == End_y ->
            {degeneratedeliveryerror,false};
        _ ->
            {ok,true}
    end.

    


