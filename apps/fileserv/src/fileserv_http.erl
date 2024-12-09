-module(fileserv_http).
-export([
    listener_spec/2
]).

-export([init/2]).

routes(Config) -> [{'_', [
    {"/[...]", fileserv_http, Config}
]}].

listener_spec(Port, Opts) ->
    Dispatch = cowboy_router:compile(routes(Opts)),
    ranch:child_spec({?MODULE, Port}, 
        ranch_tcp, [{port, Port}], 
        cowboy_clear, #{
            env => #{
                dispatch => Dispatch
            }
        }
    ).


-include("fileserv_internal.hrl").

init(Req, Opts) ->
    Method = binary_to_existing_atom(string:lowercase(cowboy_req:method(Req)), utf8),
    handle(Method, Req, Opts).


handle(M, #{path_info := Path} = Req, #{timeout := Timeout, device := Device} = Opts) 
    when M == head andalso length(Path) >= 1 
->
    DeviceOpts = maps:get(device_opts, Opts, #{}),
    ?LOG_DEBUG(#{request => head, path => Path, device => {Device, DeviceOpts}}),
    maybe 
        {ok, Pid} = fileserv_task:start(),
        {ok, Etag, Size} ?= fileserv_task:do(Pid, fun (_) -> 
            maybe
                {ok, File} ?= Device:open(Path, DeviceOpts),
                {ok, #fs_info{size = S}} ?= Device:info(File),
                {ok, Hash} ?= hash_file_loop(Device, File, crypto:hash_init(md5), 0, Opts),
                {ok, Hash, S}
            end
        end, Timeout),
        {ok, cowboy_req:reply(204, #{
                ~"etag" => binary:encode_hex(Etag),
                ~"content-length" => integer_to_binary(Size)
            }, Req), {}}
    else 
        {error, timeout} ->
            {ok, cowboy_req:reply(~"503 Overloaded", #{}, Req), {}};
        {error, enoent} ->
            {ok, cowboy_req:reply(404, Req), {}};
        {error, Other} ->
            {ok, cowboy_req:reply(502, #{~"x-reason" => io_lib:format("~tp", [Other])}, Req)}
    end;

handle(_, Req, Opts) ->
    ?LOG_ERROR(#{
        message => not_found,
        opts => Opts, 
        path => cowboy_req:path_info(Req), 
        method => cowboy_req:method(Req)
    }),
    {ok, cowboy_req:reply(404, Req), {}}.


hash_file_loop(Device, File, HashCtx, Offset, Opts) -> 
    BlockSize = 3000,
    maybe 
        {ok, Block} ?= Device:pread(File, Offset, BlockSize),
        hash_file_loop(Device, File, crypto:hash_update(HashCtx, Block), Offset+byte_size(Block), Opts)
    else 
        eof -> {ok, crypto:hash_final(HashCtx)};
        {error, _} = Err -> Err 
    end. 
