-module(toxicity_detection).
-export([detect_toxicity/1]).

detect_toxicity(Text) ->
    Url = "http://127.0.0.1:8000/api/toxicity",
    Headers = [{"Content-Type", "application/json"}],
    BinaryText = case is_binary(Text) of
                    true -> Text;
                    false -> list_to_binary(Text)
                 end,
    JsonPayload = jsx:encode([{<<"sentence">>, BinaryText}]),  
    case httpc:request(post, {Url, Headers, "application/json", JsonPayload}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            jsx:decode(list_to_binary(ResponseBody), [return_maps]);
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.