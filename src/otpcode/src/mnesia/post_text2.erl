-module(post_text2).
-author("Zaryn Technologies").
-export([
    translate/1, translate/3, 
    translate_simple/1, translate_simple/3,
    translate_print/1, translate_print/3,
    translate_show/1, translate_show/3,
    translate_only/1, translate_only/3,
    t/1, t/3,
    health_check/0, 
    get_supported_languages/0,
    test_unicode/0,
    translate_only_print/1, 
    translate_only_print/3
]).

translate(Text) ->
    translate(Text, "en", "es").

translate(Text, SrcLang, DestLang) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    
    Url = "http://localhost:8089/translate",
    
    JsonData = jsx:encode(#{
        <<"text">> => unicode:characters_to_binary(Text, utf8),
        <<"src_lang">> => list_to_binary(SrcLang),
        <<"dest_lang">> => list_to_binary(DestLang)
    }),
    
    Headers = [{"Content-Type", "application/json; charset=utf-8"}],
    
    case httpc:request(post, {Url, Headers, "application/json", JsonData}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"translated_text">> := TranslatedText,
                  <<"processing_time">> := ProcessingTime,
                  <<"model_used">> := ModelUsed} ->
                    {ok, #{
                        translated_text => TranslatedText,  
                        processing_time => ProcessingTime,
                        model_used => binary_to_list(ModelUsed)
                    }};
                _ ->
                    {error, invalid_json_response}
            end;
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {http_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

translate_simple(Text) ->
    translate_simple(Text, "en", "es").

translate_simple(Text, SrcLang, DestLang) ->
    case translate(Text, SrcLang, DestLang) of
        {ok, #{translated_text := TranslatedText}} ->
            TranslatedText;  
        {error, Reason} ->
            {error, Reason}
    end.

t(Text) ->
    t(Text, "en", "es").

t(Text, SrcLang, DestLang) ->
    translate_only(Text, SrcLang, DestLang).

translate_only(Text) ->
    translate_only(Text, "en", "es").

translate_only(Text, SrcLang, DestLang) ->
    maybe_enable_unicode_shell(),
    case translate_as_string(Text, SrcLang, DestLang) of
        {ok, String} ->
            String;                 
        {error, _Reason} ->
            "Translation failed"
    end.

translate_only_print(Text) ->
    translate_only_print(Text, "en", "es").

translate_only_print(Text, SrcLang, DestLang) ->
    case translate(Text, SrcLang, DestLang) of
        {ok, #{translated_text := Bin}} ->
            io:format("~ts~n", [Bin]),
            ok; 
        _ ->
            io:format("Translation failed~n"),
            ok  
    end.


translate_show(Text) ->
    translate_show(Text, "en", "es").

translate_show(Text, SrcLang, DestLang) ->
    case translate(Text, SrcLang, DestLang) of
        {ok, #{translated_text := TranslatedText}} ->
            io:format("~ts~n", [TranslatedText]),
            {ok, TranslatedText};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

translate_print(Text) ->
    translate_print(Text, "en", "es").

translate_print(Text, SrcLang, DestLang) ->
    case translate(Text, SrcLang, DestLang) of
        {ok, #{translated_text := TranslatedText,
               processing_time := ProcessingTime,
               model_used := ModelUsed}} ->
            io:format("~n=== Translation Results ===~n"),
            io:format("Original (~s): ~ts~n", [SrcLang, Text]),
            io:format("Translated (~s): ~ts~n", [DestLang, TranslatedText]),
            io:format("Model used: ~s~n", [ModelUsed]),
            io:format("Processing time: ~.3f seconds~n", [ProcessingTime]),
            io:format("===========================~n~n"),
            {ok, TranslatedText};
        {error, Reason} ->
            io:format("Translation Error: ~p~n", [Reason]),
            {error, Reason}
    end.

binary_to_unicode_string(Binary) when is_binary(Binary) ->
    unicode:characters_to_list(Binary, utf8).

display_utf8(Binary) when is_binary(Binary) ->
    io:format("~ts~n", [Binary]).

translate_as_string(Text, SrcLang, DestLang) ->
    case translate(Text, SrcLang, DestLang) of
        {ok, #{translated_text := TranslatedBinary}} ->
            {ok, unicode:characters_to_list(TranslatedBinary, utf8)};
        Error ->
            Error
    end.

test_unicode() ->
    TestCases = [
        {"Hello world!", "en", "es"},
        {"I love you", "en", "fr"},
        {"Good morning", "en", "de"},
        {"How are you?", "en", "ru"}
    ],
    
    io:format("~n=== Unicode Translation Test ===~n"),
    lists:foreach(fun({Text, Src, Dest}) ->
        case translate(Text, Src, Dest) of
            {ok, #{translated_text := Translated}} ->
                io:format("~s -> ~s: ~ts~n", [Src, Dest, Translated]);
            {error, Error} ->
                io:format("~s -> ~s: ERROR ~p~n", [Src, Dest, Error])
        end
    end, TestCases),
    io:format("===============================~n~n").

health_check() ->
    application:ensure_all_started(inets),
    Url = "http://localhost:8089/health",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                JsonMap ->
                    {ok, JsonMap};
                _ ->
                    {error, invalid_json}
            end;
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {http_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_supported_languages() ->
    application:ensure_all_started(inets),
    Url = "http://localhost:8089/supported-languages",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                JsonMap ->
                    {ok, JsonMap};
                _ ->
                    {error, invalid_json}
            end;
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {http_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

maybe_enable_unicode_shell() ->
    _ = (catch io:setopts(standard_io, [{encoding, unicode}])),
    _ = (catch io:setopts(standard_error, [{encoding, unicode}])),
    _ = (catch io:setopts(group_leader(), [{encoding, unicode}])),
    case erlang:function_exported(shell, strings, 1) of
        true  -> catch shell:strings(unicode);
        false ->
            catch application:load(stdlib),
            catch application:set_env(stdlib, shell_strings, unicode)
    end,
    ok.
