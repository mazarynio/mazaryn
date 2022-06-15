-module(filedata).
-export([read/1]).

read(File) ->
    file:open(File, read).