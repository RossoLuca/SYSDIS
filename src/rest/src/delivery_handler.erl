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




    db_initializer:add(#delivery{
        id=Id,
        pid = list_to_atom(binary_to_list(Pid)),
        stato = list_to_atom(binary_to_list(Stato)),
        start_x = Start_x,
        start_y = Start_y,
        current_x = Current_x,
        current_y = Current_y,
        end_x = End_x,
        end_y = End_y
    }),
	Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Data), Req1),
    {ok, Req, State0};

init( Req0=#{method := <<"GET">>}, State0 ) ->
    Qs = cowboy_req:qs(Req0),
    MatchHead = #delivery{id='$1', _='_'},
    Guard = {'=', '$1',Qs},
    Result = ['$_'],
    {atomic,Data} = db_initializer:select(delivery,[{MatchHead, [Guard],[Result]}]),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Data), Req0),
    {ok, Req, State0};


init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST,GET">>
    }, Req0),
    {ok, Req, State}.   

