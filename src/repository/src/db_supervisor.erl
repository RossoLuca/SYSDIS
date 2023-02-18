-module(db_supervisor).
-export([start/0]).

-behaviour(supervisor).
-export([init/1]).

start() ->
    db_initializer:init(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) -> 
    Specs = #{strategy => one_for_one,
        intensity => 30,
        period =>  60,
        auto_shutdown => never},
    Children = lists:map(fun(Name) ->
                            #{id => Name, start => {db_listener, start_link, [Name]}}
                        end, ['db_listener']),
    {ok, {Specs, Children}}.
