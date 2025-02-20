-module(user_dataset).
-author("Zaryn Technologies").
-export([serialize_user/1, save_to_tfrecord/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_user({user, Id, AiUserId, BusinessId, AdsId, QuantumId, Username, Password, Email, Address, Knode, Media, Post, BlogPost, Notif, Following, Follower, Blocked, SavedPosts, OtherInfo, Private, DateCreated, DateUpdated, AvatarUrl, BannerUrl, TokenId, Chat, Verified, Report, Level, LastActivity, Suspend, Data}) ->

    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

    ToBinary = fun
        (Value) when is_binary(Value) ->
            case is_valid_utf8(Value) of
                true -> Value; 
                false -> base64:encode(Value) 
            end;
        (Value) when is_list(Value) -> list_to_binary(Value); 
        (Value) when is_atom(Value) -> list_to_binary(atom_to_list(Value)); 
        (Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value)); 
        (Value) -> list_to_binary(lists:flatten(io_lib:format("~p", [Value]))) 
    end,

    #{
        <<"id">> => ToBinary(Id),
        <<"ai_user_id">> => ToBinary(AiUserId),
        <<"business_id">> => ToBinary(BusinessId),
        <<"ads_id">> => ToBinary(AdsId),
        <<"quantum_id">> => ToBinary(QuantumId),
        <<"username">> => ToBinary(Username),
        <<"password">> => ToBinary(Password),
        <<"email">> => ToBinary(Email),
        <<"address">> => ToBinary(Address),
        <<"knode">> => ToBinary(Knode), 
        <<"media">> => ToBinary(Media),
        <<"post">> => ToBinary(Post),
        <<"blog_post">> => ToBinary(BlogPost),
        <<"notif">> => ToBinary(Notif),
        <<"following">> => ToBinary(Following),
        <<"follower">> => ToBinary(Follower),
        <<"blocked">> => ToBinary(Blocked),
        <<"saved_posts">> => ToBinary(SavedPosts),
        <<"other_info">> => ToBinary(OtherInfo),
        <<"private">> => ToBinary(Private),
        <<"date_created">> => list_to_binary(TimestampStr),
        <<"date_updated">> => ToBinary(DateUpdated),
        <<"avatar_url">> => ToBinary(AvatarUrl),
        <<"banner_url">> => ToBinary(BannerUrl),
        <<"token_id">> => ToBinary(TokenId),
        <<"chat">> => ToBinary(Chat),
        <<"verified">> => ToBinary(Verified),
        <<"report">> => ToBinary(Report),
        <<"level">> => ToBinary(Level),
        <<"last_activity">> => ToBinary(LastActivity),
        <<"suspend">> => ToBinary(Suspend)
    }.

is_valid_utf8(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, _, _} -> false;
        {error, _, _} -> false;
        _ -> true
    end.

save_to_tfrecord(User, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_user(User),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_user_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("User saved to ~s~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error writing to file ~s: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end;
                {response, nofin, StatusCode, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    io:format("Error from Python service: ~p ~p~n", [StatusCode, RespBody]),
                    {error, RespBody};
                {error, Reason} ->
                    io:format("Error calling Python service: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start gun: ~p~n", [Reason]),
            {error, Reason}
    end.