-module(ipfs_client_8).
-author("Zaryn Technologies").
-export([files_chmod/2, files_touch/1, files_touch/2, key_sign/1, key_sign/2, key_verify/2, key_verify/3, log_tail/0, mount/0, mount/2, name_inspect/1,
name_inspect/2, name_inspect/3, name_pubsub_cancel/1, name_pubsub_cancel/2, name_pubsub_state/0, name_pubsub_state/1, name_pubsub_subs/0, name_pubsub_subs/1,
name_pubsub_subs/2, p2p_close/1, p2p_forward/3, p2p_forward/4, p2p_forward/5, p2p_listen/2, p2p_listen/3, p2p_listen/4, p2p_ls/0, p2p_ls/1, p2p_ls/2]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_TIMEOUT, 30000).


%% @doc Change file permissions (EXPERIMENTAL)
%% Required:
%%   - Mode: POSIX permissions mode (e.g., "0777")
%%   - Path: Path to modify
%% Example:
%% ok = ipfs_client_8:files_chmod("0755", "/path/to/file").
files_chmod(Mode, Path) when (is_list(Mode) orelse is_binary(Mode)),
                            (is_list(Path) orelse is_binary(Path)) ->
    try
        QueryString = build_query_string([{arg, Mode}, {arg, Path}]),
        Url = ?RPC_API ++ "/v0/files/chmod" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _}} ->
                ok;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Update file modification time (EXPERIMENTAL)
%% Required:
%%   - Path: Path to modify
%% Options:
%%   - mtime (integer): Modification time in seconds since Unix epoch
%%   - mtime_nsecs (integer): Nanosecond fraction of modification time
%% Example:
%% ok = ipfs_client_8:files_touch("/path/to/file", [{mtime, 1672531200}]).
files_touch(Path) ->
    files_touch(Path, []).

files_touch(Path, Options) when (is_list(Path) orelse is_binary(Path)), is_list(Options) ->
    try
        Defaults = [
            {mtime, undefined},
            {'mtime-nsecs', undefined}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/files/touch" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _}} ->
                ok;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Sign data with a key (EXPERIMENTAL)
%% Required:
%%   - Data: Binary data to sign
%% Options:
%%   - key (string): Name of key to use (default: "self")
%%   - ipns_base (string): Key encoding (default: "base36")
%% Example:
%% {ok, #{key := KeyInfo, signature := Sig}} = ipfs_client_8:key_sign(Data, [{key, "mykey"}]).
key_sign(Data) ->
    key_sign(Data, []).

key_sign(Data, Options) when is_binary(Data), is_list(Options) ->
    try
        Defaults = [
            {key, "self"},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    "self" when Key =:= key -> false;
                    "base36" when Key =:= 'ipns-base' -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/key/sign" ++ QueryString,
        
        Boundary = "------IPFSBoundary" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        BodyParts = [
            "--" ++ Boundary ++ "\r\n",
            "Content-Disposition: form-data; name=\"file\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data,
            "\r\n--" ++ Boundary ++ "--\r\n"
        ],
        Body = iolist_to_binary(BodyParts),
        
        Headers = [{"Content-Type", ContentType}],
        
        case httpc:request(post, 
                         {Url, Headers, ContentType, Body},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Key">> := _, <<"Signature">> := _} = Result ->
                        {ok, #{key => maps:get(<<"Key">>, Result),
                               signature => maps:get(<<"Signature">>, Result)}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Verify signed data (EXPERIMENTAL)
%% Required:
%%   - Data: Original binary data
%%   - Signature: Signature to verify
%% Options:
%%   - key (string): Name of key to use (default: "self")
%%   - ipns_base (string): Key encoding (default: "base36")
%% Example:
%% {ok, #{key := KeyInfo, valid := true}} = ipfs_client_8:key_verify(Data, Sig, [{key, "mykey"}]).
key_verify(Data, Signature) ->
    key_verify(Data, Signature, []).

key_verify(Data, Signature, Options) when is_binary(Data), 
                                        (is_list(Signature) orelse is_binary(Signature)), 
                                        is_list(Options) ->
    try
        Defaults = [
            {key, "self"},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        FormattedSig = case Signature of
            <<"uEJw", _/binary>> -> Signature; 
            _ -> base64:encode(Signature) 
        end,
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Key of
                    signature -> {true, {Key, FormattedSig}};
                    _ -> 
                        case Value of
                            "self" when Key =:= key -> false;
                            "base36" when Key =:= 'ipns-base' -> false;
                            _ -> {true, {Key, Value}}
                        end
                end
            end, [{signature, Signature}|MergedOpts]),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/key/verify" ++ QueryString,
        
        Boundary = "------IPFSBoundary" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        BodyParts = [
            "--" ++ Boundary ++ "\r\n",
            "Content-Disposition: form-data; name=\"file\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data,
            "\r\n--" ++ Boundary ++ "--\r\n"
        ],
        Body = iolist_to_binary(BodyParts),
        
        Headers = [{"Content-Type", ContentType}],
        
        case httpc:request(post, 
                         {Url, Headers, ContentType, Body},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Key">> := Key, <<"SignatureValid">> := Valid} ->
                        {ok, #{key => Key, valid => Valid}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                case jiffy:decode(ErrorBody, [return_maps]) of
                    #{<<"Message">> := Msg} ->
                        {error, {api_error, Msg}};
                    _ ->
                        {error, {status_code, StatusCode, ErrorBody}}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Tail the IPFS log (EXPERIMENTAL)
%% Returns a text/plain stream of log events
%% Example:
%% {ok, LogStream} = ipfs_client_8:log_tail().
log_tail() ->
    try
        Url = ?RPC_API ++ "/v0/log/tail",
        
        % IPFS expects a POST request for /log/tail
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, infinity},
                          {stream, self()}],   
                         []) of
            {ok, RequestId} when is_reference(RequestId) ->
                {ok, RequestId};
            {ok, {{_, 200, _}, Headers, Body}} ->
                case proplists:get_value("content-type", Headers) of
                    "text/plain" ++ _ ->
                        {ok, Body};
                    _ ->
                        case jiffy:decode(Body, [return_maps]) of
                            #{<<"Message">> := Msg} ->
                                {error, {api_error, Msg}};
                            Other ->
                                {error, {unexpected_response, Other}}
                        end
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Mount IPFS to filesystem (EXPERIMENTAL)
%% Default mount with no arguments
%% Example:
%% {ok, MountInfo} = ipfs_client_8:mount().
mount() ->
    mount([], []).

%% @doc Mount IPFS to filesystem with custom paths (EXPERIMENTAL)
%% Options:
%%   - ipfs_path: Path where IPFS should be mounted
%%   - ipns_path: Path where IPNS should be mounted
%% Example:
%% {ok, MountInfo} = ipfs_client_8:mount("/mnt/ipfs", "/mnt/ipns").
mount(IpfsPath, IpnsPath) when (is_list(IpfsPath) orelse is_binary(IpfsPath)),
                              (is_list(IpnsPath) orelse is_binary(IpnsPath)) ->
    try
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    [] -> false;
                    <<>> -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [{"ipfs-path", IpfsPath}, {"ipns-path", IpnsPath}]),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/mount" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"FuseAllowOther">> := AllowOther,
                      <<"IPFS">> := IPFS,
                      <<"IPNS">> := IPNS} ->
                        {ok, #{fuse_allow_other => AllowOther,
                               ipfs => IPFS,
                               ipns => IPNS}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

name_inspect(RecordData) ->
    name_inspect(RecordData, [], []).

%% @doc Inspect an IPNS record with options
%% Required: Binary record data
%% Options:
%%   - verify: CID of public IPNS key to validate against
%%   - dump: Include full hex dump (default: true)
name_inspect(RecordData, Options) when is_list(Options) ->
    name_inspect(RecordData, Options, []);
name_inspect(RecordData, Timeout) when is_integer(Timeout) ->
    name_inspect(RecordData, [], [{timeout, Timeout}]).

%% @doc Inspect an IPNS record with options and timeout
%% Required: Binary record data
name_inspect(RecordData, Options, TimeoutOpts) ->
    try
        Timeout = proplists:get_value(timeout, TimeoutOpts, ?DEFAULT_TIMEOUT),
        VerifyKey = proplists:get_value(verify, Options),
        Dump = proplists:get_value(dump, Options, true),

        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [{verify, VerifyKey}, {dump, Dump}]),

        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/name/inspect" ++ QueryString,

        Boundary = "------IPFSBoundary" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        BodyParts = [
            "--" ++ Boundary ++ "\r\n",
            "Content-Disposition: form-data; name=\"record\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            RecordData,
            "\r\n--" ++ Boundary ++ "--\r\n"
        ],
        Body = iolist_to_binary(BodyParts),

        case httpc:request(post, 
                         {Url, [{"Content-Type", ContentType}], ContentType, Body},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                decode_inspect_response(ResponseBody);
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

decode_inspect_response(ResponseBody) ->
    try jiffy:decode(ResponseBody, [return_maps]) of
        #{<<"Entry">> := Entry,
          <<"HexDump">> := HexDump,
          <<"PbSize">> := PbSize,
          <<"SignatureType">> := SigType,
          <<"Validation">> := Validation} ->
            {ok, #{
                entry => decode_entry(Entry),
                hex_dump => HexDump,
                pb_size => PbSize,
                signature_type => SigType,
                validation => decode_validation(Validation)
            }};
        Other ->
            {error, {unexpected_response, Other}}
    catch
        error:Reason ->
            {error, {decode_failed, Reason}}
    end.

decode_entry(#{<<"Sequence">> := Seq,
               <<"TTL">> := TTL,
               <<"Validity">> := Valid,
               <<"ValidityType">> := ValidType,
               <<"Value">> := Value}) ->
    #{sequence => Seq,
      ttl => TTL,
      validity => Valid,
      validity_type => ValidType,
      value => Value}.

decode_validation(#{<<"Name">> := Name,
                    <<"Reason">> := Reason,
                    <<"Valid">> := Valid}) ->
    #{name => Name,
      reason => Reason,
      valid => Valid}.

%% @doc Cancel a name subscription with default timeout
%% Required: Name to cancel
name_pubsub_cancel(Name) ->
    name_pubsub_cancel(Name, ?DEFAULT_TIMEOUT).

%% @doc Cancel a name subscription with custom timeout
%% Required: Name to cancel
name_pubsub_cancel(Name, Timeout) ->
    try
        QueryString = build_query_string([{arg, Name}]),
        Url = ?RPC_API ++ "/v0/name/pubsub/cancel" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Canceled">> := Canceled} ->
                        {ok, #{canceled => Canceled}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Query IPNS pubsub state with default timeout
name_pubsub_state() ->
    name_pubsub_state(?DEFAULT_TIMEOUT).

%% @doc Query IPNS pubsub state with custom timeout
name_pubsub_state(Timeout) ->
    try
        Url = ?RPC_API ++ "/v0/name/pubsub/state",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Enabled">> := Enabled} ->
                        {ok, #{enabled => Enabled}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Show current name subscriptions with default options
name_pubsub_subs() ->
    name_pubsub_subs([], ?DEFAULT_TIMEOUT).

%% @doc Show current name subscriptions with options
name_pubsub_subs(Options) when is_list(Options) ->
    name_pubsub_subs(Options, ?DEFAULT_TIMEOUT);
name_pubsub_subs(Timeout) when is_integer(Timeout) ->
    name_pubsub_subs([], Timeout).

%% @doc Show current name subscriptions with options and timeout
name_pubsub_subs(Options, Timeout) ->
    try
        IpnsBase = proplists:get_value('ipns-base', Options, "base36"),
        
        QueryParams = case IpnsBase of
            "base36" -> []; % Default value, no need to include
            _ -> [{'ipns-base', IpnsBase}]
        end,
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/name/pubsub/subs" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Strings">> := Strings} ->
                        {ok, #{subscriptions => Strings}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Close P2P listeners with options
%% Options:
%%   - all: boolean() - Close all listeners
%%   - protocol: string() - Match protocol name
%%   - listen_address: string() - Match listen address
%%   - target_address: string() - Match target address
p2p_close(Options) when is_list(Options) ->
    try
        Timeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
        All = proplists:get_value(all, Options),
        Protocol = proplists:get_value(protocol, Options),
        ListenAddr = proplists:get_value(listen_address, Options),
        TargetAddr = proplists:get_value(target_address, Options),

        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [
                {all, All},
                {protocol, Protocol},
                {'listen-address', ListenAddr},
                {'target-address', TargetAddr}
            ]),

        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/p2p/close" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                try binary_to_integer(ResponseBody) of
                    Count when Count >= 0 ->
                        {ok, Count};
                    _ ->
                        {error, {invalid_response, ResponseBody}}
                catch
                    error:_ ->
                        {error, {invalid_response, ResponseBody}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Forward connections to libp2p service with default options
%% Required:
%%   - Protocol: string() - Protocol name
%%   - ListenAddress: string() - Listening endpoint
%%   - TargetAddress: string() - Target endpoint
p2p_forward(Protocol, ListenAddress, TargetAddress) ->
    p2p_forward(Protocol, ListenAddress, TargetAddress, [], ?DEFAULT_TIMEOUT).

%% @doc Forward connections with options
p2p_forward(Protocol, ListenAddress, TargetAddress, Options) when is_list(Options) ->
    p2p_forward(Protocol, ListenAddress, TargetAddress, Options, ?DEFAULT_TIMEOUT);
p2p_forward(Protocol, ListenAddress, TargetAddress, Timeout) when is_integer(Timeout) ->
    p2p_forward(Protocol, ListenAddress, TargetAddress, [], Timeout).

%% @doc Forward connections with options and timeout
p2p_forward(Protocol, ListenAddress, TargetAddress, Options, Timeout) ->
    try
        AllowCustom = proplists:get_value(allow_custom_protocol, Options, false),

        BaseParams = [
            {arg, Protocol},
            {arg, ListenAddress},
            {arg, TargetAddress}
        ],
        ExtraParams = case AllowCustom of
            true -> [{'allow-custom-protocol', true}];
            false -> []
        end,

        QueryString = build_query_string(BaseParams ++ ExtraParams),
        Url = ?RPC_API ++ "/v0/p2p/forward" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "text/plain", ""},
                         [{timeout, Timeout}],
                         []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                {ok, ResponseBody};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Create libp2p service with default options
p2p_listen(Protocol, TargetAddress) ->
    p2p_listen(Protocol, TargetAddress, [], ?DEFAULT_TIMEOUT).

%% @doc Create libp2p service with options
p2p_listen(Protocol, TargetAddress, Options) when is_list(Options) ->
    p2p_listen(Protocol, TargetAddress, Options, ?DEFAULT_TIMEOUT);
p2p_listen(Protocol, TargetAddress, Timeout) when is_integer(Timeout) ->
    p2p_listen(Protocol, TargetAddress, [], Timeout).

%% @doc Create libp2p service with options and timeout
p2p_listen(Protocol, TargetAddress, Options, Timeout) ->
    try
        AllowCustom = proplists:get_value(allow_custom_protocol, Options, false),
        ReportPeer = proplists:get_value(report_peer_id, Options, false),

        BaseParams = [
            {arg, Protocol},
            {arg, TargetAddress}
        ],
        ExtraParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [
                {'allow-custom-protocol', AllowCustom},
                {'report-peer-id', ReportPeer}
            ]),

        QueryString = build_query_string(BaseParams ++ ExtraParams),
        Url = ?RPC_API ++ "/v0/p2p/listen" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "text/plain", ""},
                         [{timeout, Timeout}],
                         []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                {ok, ResponseBody};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc List active p2p listeners with default options
p2p_ls() ->
    p2p_ls([], ?DEFAULT_TIMEOUT).

%% @doc List active p2p listeners with options
p2p_ls(Options) when is_list(Options) ->
    p2p_ls(Options, ?DEFAULT_TIMEOUT);
p2p_ls(Timeout) when is_integer(Timeout) ->
    p2p_ls([], Timeout).

%% @doc List active p2p listeners with options and timeout
p2p_ls(Options, Timeout) ->
    try
        Headers = proplists:get_value(headers, Options, false),

        QueryParams = case Headers of
            true -> [{headers, true}];
            false -> []
        end,

        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/p2p/ls" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Listeners">> := Listeners} ->
                        {ok, lists:map(fun decode_listener/1, Listeners)};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

decode_listener(#{<<"Protocol">> := Proto,
                 <<"ListenAddress">> := Listen,
                 <<"TargetAddress">> := Target}) ->
    #{protocol => Proto,
      listen_address => Listen,
      target_address => Target}.

build_query_string(Options) ->
    build_query_string(Options, "").

build_query_string([], Acc) ->
    Acc;
build_query_string([{Key, Value}|Rest], Acc) ->
    Separator = case Acc of
        "" -> "?";
        _ -> "&"
    end,
    NewAcc = Acc ++ Separator ++ to_string(Key) ++ "=" ++ to_string(Value),
    build_query_string(Rest, NewAcc).

to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(true) ->
    "true";
to_string(false) ->
    "false".

merge_options(Defaults, Options) ->
    lists:ukeymerge(1, 
        lists:ukeysort(1, Options), 
        lists:ukeysort(1, Defaults)).