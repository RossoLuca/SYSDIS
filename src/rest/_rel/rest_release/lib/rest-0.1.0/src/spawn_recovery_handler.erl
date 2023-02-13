-module(spawn_recovery_handler).
-behavior(cowboy_handler).
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"POST">>}, State ) ->
	{ok, Data, Req1} = cowboy_req:read_body(Req0),
    


    DecodedTuple = jiffy:decode( Data ),
	{ [	{ <<"id">>, Id },
		{ <<"pid">>, Pid },
		{ <<"stato">>, Stato },
		{ <<"current_x">>, Current_x },
		{ <<"current_y">>, Current_y },
		{ <<"end_x">>, End_x },
		{ <<"end_y">>, End_y }
    ] } = DecodedTuple, 

    db_initializer:add(#delivery{
        id=Id,
        pid = list_to_atom(binary_to_list(Pid)),
        stato = list_to_atom(binary_to_list(Stato)),
        start_x = Current_x,
        start_y = Current_y,
        current_x = Current_x,
        current_y = Current_y,
        end_x = End_x,
        end_y = End_y
    }),

	Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, "", Req1),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.