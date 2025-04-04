-module(ipfs_media).
-author("Zaryn Technologies").
-export([upload_media/1, upload_media/2, get_media/1]).

%% Upload media with original filename
upload_media(FilePath) ->
    Filename = filename:basename(FilePath),
    upload_media(FilePath, Filename).

%% Upload media with custom filename
upload_media(FilePath, CustomFilename) ->
    case file:read_file(FilePath) of
        {ok, MediaData} ->
            case ipfs_client_1:block_put(CustomFilename, MediaData) of
                {ok, #{cid := CID}} ->
                    binary_to_list(CID);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% Get Media using CID 
get_media(CID) ->
   {ok, Media} = ipfs_client_1:block_get(CID),
   Media.

