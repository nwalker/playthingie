-module(fileserv).

% application
-export([start/2, stop/1]).
% supervisor
-export([init/1]).

% dev utils
-export([
    add_listener/2,
    restart/0,
    r/0
]).

start() -> 
    application:ensure_all_started(fileserv).

stop() -> 
    application:stop(fileserv),
    application:unload(fileserv).

restart() ->
    stop(),
    start().

r() ->
    (catch r3:compile()),
    restart().

-define(SUP, fileserv_sup).

add_listener(Port, Opts) ->
    OptsWithDefaults = maps:merge(#{
        timeout => 3000,
        device => fileserv_fs
    }, Opts),
    supervisor:start_child(?SUP, fileserv_http:listener_spec(Port, OptsWithDefaults)).


% ============ OTP things ============ 
% -behaviour(application).

start(_StartType, _StartArgs) ->
    {ok, Sup} = supervisor:start_link({local, ?SUP}, ?MODULE, []),
    add_listener(12080, #{}),
    add_listener(12081, #{device => fileserv_fs_dummy}),
    {ok, Sup}.

stop(_State) ->
    ok.


% -behaviour(supervisor).

init([]) ->
    %% sup_flags() = #{strategy => strategy(),         % optional
    %%                 intensity => non_neg_integer(), % optional
    %%                 period => pos_integer()}        % optional
    %% child_spec() = #{id => child_id(),       % mandatory
    %%                  start => mfargs(),      % mandatory
    %%                  restart => restart(),   % optional
    %%                  shutdown => shutdown(), % optional
    %%                  type => worker(),       % optional
    %%                  modules => modules()}   % optional
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        fileserv_task:child_spec()
    ],
    {ok, {SupFlags, ChildSpecs}}.
