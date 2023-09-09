-module(chatbot).
-export([call_python/1]).

call_python(Prompt) ->
    Command = "python3 mazaryn/src/otpcode/pycode/chat/chatgpt.py '" ++ Prompt ++ "'",
    Response = os:cmd(Command),
    Response.
