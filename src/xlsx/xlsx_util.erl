-module(xlsx_util).

-export([
    new/1,
    write/2,
    mkdir/2,
    get_output_stream/2,
    write/3,
    get_sheets/1
]).

-record(xlsx, {tmp, files = [], sheets = []}).


%%--------------------------------------------------------------------
%% @doc
%% generate xlsx record and create tmp folder
%%
%% @end
%%--------------------------------------------------------------------
-spec new(OutFile :: file:filename_all()) -> Xlsx :: #xlsx{}.
new(OutFile) ->
    TmpDir = temp_file(OutFile),
%%    error_logger:info_msg("TmpDir ~p~n", [TmpDir]),
    ok = mkdir("", TmpDir),
    #xlsx{tmp = TmpDir}.


%%--------------------------------------------------------------------
%% @doc
%% get Sheets list from xlsx
%%
%% @end
%%--------------------------------------------------------------------
-spec get_sheets(Xlsx :: #xlsx{}) -> Sheets :: list().
get_sheets(#xlsx{sheets = Sheets}) ->
    Sheets.


%%--------------------------------------------------------------------
%% @doc
%% zip files to xlsx file
%%
%% @end
%%--------------------------------------------------------------------
-spec write(Xlsx :: #xlsx{}, OutFile :: file:filename_all()) -> ok | {error, Reason :: term()}.
write(#xlsx{tmp = Tmp, files = Files}, OutFile) ->
%%    error_logger:info_msg("xlsx output file: ~p~nTmp Path: ~p~nFiles: ~p~n", [OutFile, Tmp, Files]),

    case zip:create(OutFile, Files, [{cwd, Tmp}]) of
        {ok, _Path} ->
            os:cmd("rm -rf " ++ Tmp),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% make dir
%%
%% @end
%%--------------------------------------------------------------------
-spec mkdir(BaseDir :: file:filename_all(), Dir :: file:filename_all()) -> ok.
mkdir(BaseDir, Dir) ->
    lists:foldl(
        fun(Part, Path) ->
            D = filename:join(Path, Part),
            case filelib:is_dir(D) of
                true ->
                    nop;
                false ->
                    file:make_dir(D)
            end,
            D
        end,
        BaseDir,
        string:tokens(Dir, "/")),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% open file stream
%%
%% @end
%%--------------------------------------------------------------------
-spec get_output_stream(X :: #xlsx{}, RefPath :: file:filename_all()) -> {ok, {F :: file:io_device(), X :: #xlsx{}}}.
get_output_stream(X = #xlsx{tmp = Tmp, files = Files}, RelPath) ->
    mkdir(Tmp, filename:dirname(RelPath)),
    Path = filename:join(Tmp, RelPath),
    {ok, F} = file:open(Path, [write]),
    {ok, {F, X#xlsx{files = [RelPath | Files]}}}.


%%--------------------------------------------------------------------
%% @doc
%% write file to target path
%%
%% @end
%%--------------------------------------------------------------------
-spec write(X :: #xlsx{}, RefPath :: file:filename_all(), Bytes :: [iodata()]) -> {ok, X :: #xlsx{}}.
write(X, RelPath, Bytes) ->
    {ok, {F, X2}} = get_output_stream(X, RelPath),
    ok = file:write(F, Bytes),
    ok = file:close(F),
    {ok, X2}.

%% helpers


%%--------------------------------------------------------------------
%% @doc
%% generate temp file
%%
%% @end
%%--------------------------------------------------------------------
-spec temp_file(OutFile :: file:filename_all()) -> FileName :: file:filename_all().
temp_file(OutFile) ->
    {A, B, C} = os:timestamp(),
    filename:join(
        filename:dirname(OutFile),
        lists:flatten(io_lib:format("xlsx-~s-~p.~p.~p", [node(), A, B, C]))
    ).