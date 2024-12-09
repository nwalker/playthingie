-module(fileserv_SUITE).
-compile([export_all]).


all() -> [
    {group, basic}, 
    {group, overload_always},
    {group, overload_1sec}
].

groups() -> [
    {basic, [parallel], [basic_success, basic_404]},
    {overload_always, [], [overload_success]},
    {overload_1sec, [], [overload_success]}
].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fileserv),
    inets:start(httpc, []), 
    Config.

end_per_suite(_Config) -> ok.

init_per_group(basic, Config) ->
    Root = proplists:get_value(data_dir, Config),
    fileserv:add_listener(13857, #{device => fileserv_fs, device_opts => #{root => Root}}), 
    [{port, 13857} | Config];

init_per_group(overload_always, Config) ->
    fileserv:add_listener(13858, #{timeout => 0, device => fileserv_fs_dummy}), 
    [{port, 13858} | Config];

init_per_group(overload_1sec, Config) ->
    fileserv:add_listener(13859, #{timeout => 1000, device => fileserv_fs_dummy}), 
    [{port, 13858} | Config];

init_per_group(_, Config) -> Config.

end_per_group(_, _Config) -> ok.


basic_success(Config) ->
    {ok, Res} = request("/test_file", Config),
    #{
        status := 204, 
        headers := Hdr
    } = Res,
    KnownHash = binary:encode_hex(crypto:hash(md5, ~"0123456789")),
    KnownHash = iolist_to_binary(proplists:get_value("etag", Hdr)),
    ok.


basic_404(Config) ->
    {ok, Res} = request("/nonexistent", Config),
    #{status := 404} = Res,
    ok.


overload_success(Config) ->
    % 10GB of repeated sha256("asdasd")
    % этот тест в группе overload_1sec может мигать на слишком быстрой машине
    {ok, Res} = request("/asdasd/10G", Config),
    #{
        status := 503,
        status_line := "Overloaded"
    } = Res,
    ok.


request(Path, Config) ->
    Port = proplists:get_value(port, Config),
    URL = #{
        scheme => "http", host => "localhost", port => Port,
        path => Path
    },
    Res = httpc:request(head, {uri_string:recompose(URL), []}, [], []),
    ct:log(info, "~tp", [Res]),
    case Res of 
        {ok, {{_Vsn, Status, StatusString}, Headers, _Body}} ->
            {ok, URL#{status => Status, headers => Headers, status_line => StatusString}};
        Other -> Other 
    end.