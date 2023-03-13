-module(utils).

-export([update_personal_collisions/5, get_route_start/1, check_drone_pid/3, change_state/4, update_entry_in_collision_table/5]).

-export([send_update_table_add/4, send_update_table_remove/2]).

-export([pid_coding/1, pid_decoding/1]).

-export([stop_container/1, remove_container/1]).

% Returns the recovery_start (i.e. the last position reached by the drone) if the drone is a recovery drone
% otherwise returns the original start
get_route_start(Configuration) ->
    Recovery = maps:get(recovery, Configuration),
    if Recovery == true ->
        maps:get(recovery_start, Configuration);
    true ->
        maps:get(route_start, Configuration)
    end.

% If there's a new collision, this is added to the PersonalCollisions
% It there isn't a new collision, the entry is removed from the PersonalCollisions in the case an entry for the same id
% is already stored (it can be the case of a sync_hello received from a recovery drone for which we had found a collision in the past)
update_personal_collisions(Collision_response, FromPid, FromId, Collision_points, PersonalCollisions) ->
    NewPersonalCollisions = case Collision_response of
                        collision ->
                            maps:put(FromId, #{pid => FromPid, points => Collision_points}, PersonalCollisions);
                        no_collision ->
                            maps:remove(FromId, PersonalCollisions)
                    end,
    NewPersonalCollisions.

% Add or update the entry of the drone Id in the CollisionTable storing its CollidingDrone, State and Notify_count
update_entry_in_collision_table(Id, CollisionTable, CollidingDrones, State, Notify_count) ->
    Collisions = #{
        collisions => sets:from_list(CollidingDrones),
        state => State,
        notify_count => Notify_count
    },
    maps:put(Id, Collisions, CollisionTable).

% The personal state is updated both inside the DroneState and inside the personal entry in the CollisionTable
change_state(Id, NewState, DroneState, CollisionTable) ->
    NewDroneState = maps:put(state, NewState, DroneState),
    NewCollisionTable = maps:put(Id, maps:put(state, NewState, maps:get(Id, CollisionTable)), CollisionTable),
    {NewDroneState, NewCollisionTable}.

% Returns true if FromPid is exactly the same that is stored in the entry of FromId in the PersonalCollisions
check_drone_pid(FromPid, FromId, PersonalCollisions) ->
    FindInPersonalCollisions = maps:get(FromId, PersonalCollisions, false),
    StoredPid = if FindInPersonalCollisions =/= false ->
                    maps:get(pid, FindInPersonalCollisions);
                true ->
                    false
                end,
    PidConsistency = (StoredPid =/= false) andalso (StoredPid == FromPid),
    PidConsistency.

% Send an update_table message of type add to To which contains the collisions stored in PersonalCollisions 
% and the state, notify_count stored in DroneState
send_update_table_add(Id, To, PersonalCollisions, DroneState) ->
    CollidingDrones = maps:keys(PersonalCollisions),
    State = maps:get(state, DroneState),
    Notify_count = maps:get(notify_count, DroneState),

    To ! {update_table, self(), Id, add, CollidingDrones, State, Notify_count}.

% Send an update_table message of type remove to To which means that there's no more collision with the drone Id
send_update_table_remove(Id, To) ->
    To ! {update_table, self(), Id, remove, none, none, none}.

%% Given a Pid as input, returns its binary representation
pid_coding(Pid) ->
    BinaryPid = erlang:term_to_binary(Pid),
    ListBinaryPid = erlang:binary_to_list(BinaryPid),
    ListBinaryPid.

% Given a list of integer, that represents a binary data, returns the Pid that the binary encode
pid_decoding(ListPid) ->
    BinaryPid = erlang:list_to_binary(ListPid),
    Pid = erlang:binary_to_term(BinaryPid),
    Pid.

stop_container(Id) ->
    Command = "docker -H unix:///var/run/docker.sock stop drone_" ++ integer_to_list(Id),
    _StdOut = os:cmd(Command).

remove_container(Id) ->
    Command = "docker -H unix:///var/run/docker.sock rm drone_" ++ integer_to_list(Id),
    _StdOut = os:cmd(Command).