-module(drone_policy).

-export([compute_policy/2]).

%% Returns an ordering for the drones contained in the CollisionTable
%% Notify_threshold --> integer
%% CollisiontTable -->
%                    #{Id => 
%                        	#{collisions => sets:from_list(<List of colliding drones>),
%                        	  notify_count => <list of drones to which has been sent a notify message> --> It never contains duplicate drones
%                        	  state => pending | flying
%                        	}
%                          	....
%                        	....
%                        	....
%                    }
compute_policy(CollisionTable, Notify_Threshold) ->
    FirstRule = maps:fold(fun(K, V, AccIn) ->
                State = maps:get(state, V),
                if State == flying ->
                    AccOut = [K | AccIn],
                    AccOut;
                true ->
                    AccIn
                end
    end, [], CollisionTable),
    FirstRuleOrdered = lists:sort(FirstRule),
    TableAfterFirstRule = lists:foldl(fun(K, Acc) -> 
                            Out = maps:remove(K, Acc),
                            Out
                    end, CollisionTable, FirstRuleOrdered),
    SecondRule = maps:fold(fun(K, V, AccIn) ->
                        Count = length(maps:get(notify_count, V)),
                        if Count >= Notify_Threshold ->
                            AccOut = [K | AccIn],
                            AccOut;
                        true ->
                            AccIn
                        end
                    end, [], TableAfterFirstRule),
    SecondRuleOrdered = lists:sort(SecondRule),
    TableAfterSecondRule = lists:foldl(fun(K, Acc) -> 
                            Out = maps:remove(K, Acc),
                            Out
                    end, TableAfterFirstRule, SecondRuleOrdered),
    ThirdRuleOrdered = lists:sort(fun({A, MapA}, {B, MapB}) ->
                    CollisionA = sets:size(maps:get(collisions, MapA)),
                    CollisionB = sets:size(maps:get(collisions, MapB)),
                    if CollisionA < CollisionB ->
                        true;
                    CollisionA == CollisionB ->
                        if A < B ->
                            true;
                        true ->
                            false
                        end;
                    true ->
                        false
                    end
        end, maps:to_list(TableAfterSecondRule)),
    ThirdRuleIds = lists:map(fun({K, _V}) -> K end, ThirdRuleOrdered),
    TotalOrdering = lists:append(FirstRuleOrdered, lists:append(SecondRuleOrdered, ThirdRuleIds)),
    TotalOrdering.