-module(post_text).
-export([category/1]).

-define(API_URL, "http://127.0.0.1:8000/api/text_predict").

% Function to get the categories for a sentence
category(Sentence) ->
    JsonPayload = jsx:encode([{<<"sentence">>, list_to_binary(Sentence)}]),
    
    case httpc:request(post, {?API_URL, [], "application/json", JsonPayload}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"categories">> := Categories} ->
                    CategoryList = [binary_to_list(maps:get(<<"category">>, Cat)) || Cat <- Categories],
                    CategoryList;
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.