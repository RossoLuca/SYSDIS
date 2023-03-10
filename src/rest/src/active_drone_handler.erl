-module(active_drone_handler).
-behavior(cowboy_handler).
-include_lib("stdlib/include/qlc.hrl").
-include( "records.hrl" ).
-export([init/2]).

init( Req0=#{method := <<"GET">>}, State ) ->
    logging:log("Received GET request in active_drone_handler"),

    MatchHead = #delivery{id='$1',state='$2',pid='$3', _='_'},
    GuardActive = {'=/=', '$2',completed},
    MatchResult = ['$_'],

    MatchSpecs = [{MatchHead, [GuardActive],[MatchResult]}],
    
    {Status, Result} = mnesia_wrapper:transaction(select, delivery, MatchSpecs),

    ParsedQs = cowboy_req:parse_qs(Req0),
    AtomQs = [{binary_to_atom(K), binary_to_list(V)} || {K, V} <- ParsedQs],
    FieldsOn = lists:search(fun({K, _V}) -> 
                            if K == fields ->
                                true;
                            true ->
                                false
                            end
            end, AtomQs),
    
    case FieldsOn of
        {value, {fields, Fields}} ->
            FieldList = lists:map(fun(T) -> list_to_atom(T) end, string:tokens(Fields, ",")),
            if Status == atomic, length ->
                ResultLength = length(Result),
                if ResultLength > 0 ->
                    Keys = maps:keys(lists:nth(1, Result)),
                    Intersection = sets:intersection(sets:from_list(FieldList, Keys)),
                    IntersectionSize = sets:size(Intersection),
                    if IntersectionSize > 0 ->
                        %% adapt response to only the fields in intersection
                        Adapted = adaptResult(Intersection, Result),
                        Response = return_req(Status, Adapted, Req0),
                        {ok, Response, State};
                    true ->
                        Response = return_req(aborted,Result,Req0),
                        {ok, Response, State}
                    end;
                true ->
                    Req = return_req(Status,Result,Req0),
                    {ok, Req, State}
                end;
            true ->
                Req = return_req(Status,Result,Req0),
                {ok, Req, State}
            end;
        false ->
            Req = return_req(Status,Result,Req0),
            {ok, Req, State}
    end;
    
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"GET">>
    }, Req0),
    {ok, Req, State}.

adaptResult(Fields, Result) -> 
    Adapted = lists:map(fun(T) -> 
                        Item = lists:foldl(fun(Field, AccIn) ->
                                AccOut = maps:put(Field, maps:get(field,T), AccIn),
                                AccOut
                            end, Fields),
                        Item
            end, Result),
    Adapted.

return_req(atomic,Result,Req0)->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Result), Req0);
return_req(aborted,_,Req0)->
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, "", Req0).