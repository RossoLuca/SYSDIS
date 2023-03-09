-module(utils).

-export([update_personal_collisions/5, get_route_start/1, check_drone_pid/3, change_state/4, update_entry_in_collision_table/5]).

-export([send_update_table_add/4, send_update_table_remove/2]).

get_route_start(Configuration) ->
    Recovery = maps:get(recovery, Configuration),
    if Recovery == true ->
        maps:get(recovery_start, Configuration);
    true ->
        maps:get(route_start, Configuration)
    end.

update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions) ->
    NewPersonalCollisions = case Collision_response of
                        collision ->
                            maps:put(FromId, #{pid => FromPid, points => Collision_points}, PersonalCollisions);
                        no_collision ->
                            maps:remove(FromId, PersonalCollisions)
                    end,
    NewPersonalCollisions.

update_entry_in_collision_table(Id, CollisionTable, CollidingDrones, State, Notify_count) ->
    Collisions = #{
        collisions => sets:from_list(CollidingDrones),
        state => State,
        notify_count => Notify_count
    },
    maps:put(Id, Collisions, CollisionTable).


change_state(Id, NewState, DroneState, CollisionTable) ->
    NewDroneState = maps:put(state, NewState, DroneState),
    NewCollisionTable = maps:put(Id, maps:put(state, NewState, maps:get(Id, CollisionTable)), CollisionTable),
    {NewDroneState, NewCollisionTable}.

check_drone_pid(FromPid, FromId, PersonalCollisions) ->
    FindInPersonalCollisions = maps:get(FromId, PersonalCollisions, false),
    StoredPid = if FindInPersonalCollisions =/= false ->
                    maps:get(pid, FindInPersonalCollisions);
                true ->
                    false
                end,
    PidConsistency = (StoredPid =/= false) andalso (StoredPid == FromPid),
    PidConsistency.


send_update_table_add(Id, To, PersonalCollisions, DroneState) ->
    CollidingDrones = maps:keys(PersonalCollisions),
    State = maps:get(state, DroneState),
    Notify_count = maps:get(notify_count, DroneState),

    To ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count}.

send_update_table_remove(Id, To) ->
    To ! {update_table, self(), Id, remove, none, none, none}.