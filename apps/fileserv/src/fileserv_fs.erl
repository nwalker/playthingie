-module(fileserv_fs).

-include_lib("kernel/include/file.hrl").
-include("../src/fileserv_internal.hrl").

-export([open/2, info/1, pread/3]).

open(PathParts, Opts) ->
    maybe
        {ok, Cwd} ?= case maps:get(root, Opts, cwd) of 
            cwd -> file:get_cwd();
            Other -> {ok, Other} 
        end,
        {ok, Path} ?= case filelib:safe_relative_path(filename:join(PathParts), Cwd) of 
            unsafe -> {error, unsafe};
            Safe -> {ok, filename:join(Cwd, Safe)}
        end,
        ?LOG_DEBUG(#{driver => ?MODULE, open => Path}),
        {ok, FD} ?= file:open(Path, [raw, binary]), 
        {ok, {FD, #{}}}
    end.


info({FD, _}) ->
    maybe 
        {ok, #file_info{
            size = S, mtime = MTime
        }} ?= file:read_file_info(FD),
        {ok, #fs_info{size = S, mtime = MTime}}
    end.


pread({FD, _}, Offset, Size) ->
    file:pread(FD, Offset, Size).
