-module(create_delivery_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).
-export([init/2]).
-define(MAXSIZE,1000.0).

init( Req0=#{method := <<"POST">>}, State ) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    DecodedTuple = jiffy:decode( Data, [return_maps]),

    
    Start_x = maps:get(<<"start_x">>,DecodedTuple),
    Start_y = maps:get(<<"start_y">>,DecodedTuple),
    End_x = maps:get(<<"end_x">>,DecodedTuple),
    End_y = maps:get(<<"end_y">>,DecodedTuple),
    erlang:display(Start_x),
    erlang:display(Start_y),
    erlang:display(End_x),
    erlang:display(End_y),


    
    case coordinates_check({Start_x,Start_y,End_x,End_y}) of 
        {ok,_} ->
            Id = drone_hub_wrapper:notify(create,{Start_x,Start_y,End_x,End_y}),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{result => ok, id => Id}), Req1),
            {ok, Req, State};
        {Atom,false} ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{error => Atom}), Req1),
            {ok, Req, State}
    end;


    
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.

coordinates_check({Start_x,Start_y,End_x,End_y}) ->
    case {Start_x,End_x,Start_y,End_y} of
        {Start_x,End_x,Start_y,End_y} when Start_x < 0; Start_y < 0;End_x < 0 ; End_y < 0 ->
            {coordinateserror,false};
        {Start_x,End_x,Start_y,End_y} when Start_x > ?MAXSIZE; Start_y > ?MAXSIZE;End_x > ?MAXSIZE ; End_y > ?MAXSIZE ->
            {coordinateserror,false};
        {Start_x,End_x,Start_y,End_y} when Start_x == End_x , Start_y == End_y ->
            {degeneratedeliveryerror,false};
        _ ->
            {ok,true}
    end.