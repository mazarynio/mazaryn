-module(post_dataset).
-author("Zaryn Technologies").
-export([serialize_post/1, save_to_tfrecord/2]).

-include_lib("kernel/include/logger.hrl").
-include("../records.hrl").

serialize_post({post, Id, AiPostId, Content, Emoji, Comments, Likes, Media, Hashtag, Mention, LinkUrl, Author, _Other, DateCreated, _DateUpdated, _Report, _DeviceInfo, _Data}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateCreated,
    TimestampStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]),

    #{
        <<"id">> => list_to_binary(Id),
        <<"ai_post_id">> => list_to_binary(AiPostId),
        <<"content">> => Content,
        <<"emoji">> => list_to_binary(Emoji),
        <<"comments">> => list_to_binary(lists:flatten(io_lib:format("~p", [Comments]))),
        <<"likes">> => list_to_binary(lists:flatten(io_lib:format("~p", [Likes]))),
        <<"media">> => list_to_binary(Media),
        <<"hashtag">> => list_to_binary(Hashtag),
        <<"mention">> => list_to_binary(Mention),
        <<"link_url">> => list_to_binary(LinkUrl),
        <<"author">> => list_to_binary(Author),
        <<"date_created">> => list_to_binary(TimestampStr)
    }.

save_to_tfrecord(Post, Filename) ->
    case application:ensure_all_started(gun) of
        {ok, _} ->
            JsonData = serialize_post(Post),
            JsonDataWithFilename = JsonData#{<<"filename">> => list_to_binary(Filename)}, 

            {ok, ConnPid} = gun:open("localhost", 8000), 
            {ok, _Protocol} = gun:await_up(ConnPid),

            Headers = [{<<"content-type">>, <<"application/json">>}],
            StreamRef = gun:post(ConnPid, "/api/convert_post_to_tfrecord", Headers, jsone:encode(JsonDataWithFilename)),

            case gun:await(ConnPid, StreamRef) of
                {response, nofin, 200, _RespHeaders} ->
                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
                    case file:write_file(Filename, RespBody, [append]) of
                        ok ->
                            io:format("Post saved to ~s~n", [Filename]),
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