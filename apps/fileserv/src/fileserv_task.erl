-module(fileserv_task).

-export([child_spec/0]).
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_info/2,
    terminate/2
]).

-export([task_wrapper/1]).
-export([
    start/0,
    do/3
]).

-include("fileserv_internal.hrl").
-define(SUP, fileserv_task_sup).

child_spec() -> #{ 
    id => ?SUP,
    start => {supervisor, start_link, [{local, fileserv_task_sup}, ?MODULE, start_sup]},
    type => supervisor 
}.


start() ->
    supervisor:start_child(?SUP, [self()]).

do(TaskHolder, Fun, Timeout) ->
    case (catch gen_server:call(TaskHolder, {do, Fun}, Timeout)) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', Other} -> exit(Other);
        Result -> Result
    end.


start_link(Owner) ->
    gen_server:start_link(?MODULE, {start_task_holder, Owner}, []).


init(start_sup) ->
    Flags = #{strategy => simple_one_for_one},
    ChildSpec = #{
        id => ?MODULE, 
        start => {?MODULE, start_link, []},
        restart => temporary
    },
    {ok, {Flags, [ChildSpec]}};

init({start_task_holder, Owner}) ->
    erlang:monitor(process, Owner),
    {ok, #{owner => Owner}}.


handle_call({do, Fun}, Ref, #{} = State) ->
    {ok, Job} = proc_lib:start_link(?MODULE, task_wrapper, [{Fun, Ref}]),
    {noreply, State#{job => Job}};

handle_call(Req, Ref, State) ->
    ?LOG_ERROR(#{reason => {badcall, Req}, offender => Ref}),
    {stop, badcall, State}.

handle_info({'DOWN', _, process, Owner, Reason}, #{owner := Owner} = State) ->
    case Reason of 
        normal -> pass;
        _ -> ?LOG_DEBUG(#{message => owner_down, reason => Reason, owner => Owner})
    end,
    Job = maps:get(job, State, undefined),
    is_pid(Job) andalso exit(Job, owner_down),
    {stop, normal, #{}};

handle_info(Info, State) -> 
    ?LOG_ERROR(#{reason => {unexpected_info, Info}}),
    {noreply, State}.

terminate(_, _) -> ok. 


task_wrapper({Fun, Ref}) ->
    proc_lib:init_ack({ok, self()}),
    gen_server:reply(Ref, Fun(#{})).
