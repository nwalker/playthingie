-module(fileserv_fs_dummy).
-include("fileserv_internal.hrl").

-export([open/2, info/1, pread/3]).


-record(device, {block, block_size, size}).

open(Path, Opts) -> 
    maybe 
        {ok, {Seed, Size}} ?= parse_path(Path),
        Block = make_block(Seed, Opts),
        {ok, #device{
            block = Block,
            block_size = erlang:byte_size(Block),
            size = Size
        }}
    end.

parse_path([Seed, PathSize]) ->
    maybe 
        {ok, Size} ?= parse_size(PathSize),
        {ok, {Seed, Size}}
    end;
parse_path(_) -> 
    badpath.

parse_size(<<>>) -> {error, empty_size};

parse_size(Str) ->
    {ok, RE} = re:compile("^([0-9]+)([KMG]?)"),
    parse_size_loop(Str, 0, RE).

parse_size_loop(<<>>, Acc, _) -> {ok, Acc};

parse_size_loop(Str, Acc, RE) ->
    Res = re:run(Str, RE, [{capture, all, binary}]),
    case Res of
        nomatch -> 
            {error, {badformat, Str}};
        {match, [Prefix, V, Exp]} ->
            <<Prefix:(byte_size(Prefix))/binary, Rest/binary>> = Str,
            parse_size_loop(Rest, inc_size(V, Exp, Acc), RE)    
    end.

inc_size(V, Exp, Acc) ->
    Mult = map_get(Exp, #{
        ~"" => 1,
        ~"K" => 1024,
        ~"M" => 1024*1024,
        ~"G" => 1024*1024*1024
    }),
    Acc + erlang:binary_to_integer(V) * Mult.

make_block(<<"raw:", Block/binary>>, _) ->
    Block;

make_block(Seed, Opts) ->
    Algo = maps:get(algo, Opts, sha256),
    crypto:hash(Algo, Seed).

divmod(A, B) -> {A div B, A rem B}.

info(#device{size = S}) ->
    {ok, #fs_info{size = S, mtime = 0}}.

pread(Device, Offset, Size) ->
    #device{ 
        block = Block,
        block_size = BlockSize,
        size = DeviceSize
    } = Device,
    maybe
        ok ?= check_offset(Device, Offset),
        ok ?= check_size(Size),

        {PrefixLength, FirstBlock} = case divmod(Offset, BlockSize) of 
            {FB, 0} -> {0, FB};
            {FB, PO} -> {BlockSize-PO, FB+1}
        end,
        NewOffset = Offset + PrefixLength,
        FinalSize = min(Size - PrefixLength, DeviceSize - NewOffset),

        {SuffixLength, LastBlock} = case divmod(NewOffset + FinalSize, BlockSize) of 
            {LB, SL} -> {SL, LB-1} 
        end,
        Prefix = binary_part(Block, BlockSize, -PrefixLength),
        Suffix = binary_part(Block, 0, SuffixLength),
        BlocksToReply = lists:seq(FirstBlock, LastBlock),
        {ok, iolist_to_binary([Prefix, [Block || _ <- BlocksToReply], Suffix])}
    end.

check_offset(#device{size = Size}, Offset) when Offset < Size -> ok;
check_offset(_, _) -> eof.

check_size(-1) -> {error, badarg};
check_size(0) -> eof;
check_size(_) -> ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_size_test_( ) -> [
    ?_test( {ok, 1} = parse_size(~"1") ),
    ?_test( {ok, 1025} = parse_size(~"1K1") ),
    ?_test( {ok, 1025*1024} = parse_size(~"1M1K") ),
    ?_test( {error, {badformat, ~"asd"}} = parse_size(~"asd") ),
    ?_test( {error, {badformat, ~"asd"}} = parse_size(~"1asd") ),
    ?_test( {error, {badformat, ~"asd"}} = parse_size(~"1G1asd") ),
    ?_test( {error, {badformat, ~"asd"}} = parse_size(~"1G1Masd") ),
    ?_test( ok )
].

pread_boundaries_test_() -> 
    {ok, Device10} = open([~"raw:0123456789", ~"10"], #{}),
    {ok, Device25} = open([~"raw:0123456789", ~"25"], #{}),
    [
        ?_test( {error, badarg} = pread(Device25, 0, -1) ),
        ?_test( eof = pread(Device25, 0, 0) ),
        ?_test( eof = pread(Device25, 25, 1) ),
        ?_test( {ok, ~"56789"} 
                    = pread(Device10, 5, 10) ),

        {"prefix, no blocks, no suffix ", ?_test( 
            {ok, ~"56789"} 
                = pread(Device25, 5, 5) 
        )},
        {"no prefix, no blocks, suffix", ?_test( 
            {ok, ~"01"} 
                = pread(Device25, 10, 2) 
        )},
        {"no prefix, no blocks, suffix truncated", ?_test( 
            {ok, ~"01234"} 
                = pread(Device25, 20, 7) 
        )},
        {"prefix, no blocks, suffix", ?_test( 
            {ok, ~"90"} 
                = pread(Device25, 9, 2) 
        )},
        {"prefix, no blocks, suffix truncated", ?_test( 
            {ok, ~"901234"} 
                = pread(Device25, 19, 9) 
        )},
        {"prefix, block, no suffix", ?_test( 
            {ok, ~"90123456789"} 
                = pread(Device25, 9, 11) 
        )},
        {"no prefix, block, suffix", ?_test( 
            {ok, ~"0123456789012"} 
                = pread(Device25, 10, 13) 
        )},
        {"prefix, block, suffix", ?_test( 
            {ok, ~"9012345678901"} 
                = pread(Device25, 9, 13) 
        )},
        {"prefix, block, truncated suffix", ?_test( 
            {ok, ~"9012345678901234"}
                = pread(Device25, 9, 19) 
        )},

        {"big unaligned read", ?_test(begin
            {ok, Device} = open([~"raw:0123456789", ~"15M"], #{}),
            {ok, Data} = pread(Device, 10242, 167),
            ?assert( byte_size(Data) == 167 ),
            ?assert( crypto:hash(sha256, Data) == crypto:hash(sha256, [
                ~"23456789", 
                [~"0123456789" || _ <- lists:seq(1, 15)], 
                ~"012345678"
            ]))
        end)},
        {"big unaligned truncated read", ?_test(begin
            {ok, Device} = open([~"raw:0123456789", ~"10K155"], #{}),
            {ok, Data} = pread(Device, 10232, 1024),
            ?assert( byte_size(Data) == 163 ),
            ?assert( crypto:hash(sha256, Data) == crypto:hash(sha256, [
                ~"23456789", 
                [~"0123456789" || _ <- lists:seq(1, 15)], 
                ~"01234"
            ]))
        end)},

        ?_test( ok )
    ].

other_test_() -> [
    ?_test( {ok, _} = open([~"abcd", ~"10K"], #{}) ),
    ?_test( ok )
].

-endif.
