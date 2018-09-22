-module(linerl).
-export([main/1]).

-record(file_info, {name, size, line_count, line_lengths, filetype}).
-record(filetype_info, {file_count, line_count}).
-record(state, {info = #{}}).

-define(LOOP_TIMEOUT, 1000).

main(Args) ->
    Parser = parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

loop(#state{info = Info}=State) ->
    receive
        {file_processed, FileInfo} ->
            FileType = FileInfo#file_info.filetype,
            LineCount = FileInfo#file_info.line_count,
            NewInfo = case maps:get(FileType, Info, empty) of
                           empty ->
                              NewInfo2 = #filetype_info{file_count = 1,
                                                        line_count = LineCount},
                              maps:put(FileType, NewInfo2, Info);
                           OldInfo ->
                              #filetype_info{file_count = OldFileCount,
                                             line_count = OldLineCount} = OldInfo,
                              NewInfo2 = #filetype_info{file_count = OldFileCount + 1,
                                                        line_count = OldLineCount + LineCount},
                              maps:put(FileType, NewInfo2, Info)
                       end,
            loop(#state{info = NewInfo});
        {error, {Error, FileName}} ->
            io:format("error |~p| while processing file:~n~p~n", [Error, FileName]),
            loop(State)
    after ?LOOP_TIMEOUT ->
            InfoList = maps:to_list(Info),
            lists:foreach(fun(X) -> format_output(X) end, InfoList)
    end.

format_output({FileType, FileTypeInfo}) ->
    #filetype_info{file_count = FileCount,
                   line_count = LineCount} = FileTypeInfo,
    io:format("| ~s -> ", [FileType]),
    io:format("file count: ~p | ", [FileCount]),
    io:format("line count: ~p |~n", [LineCount]).

process_dir(DirName, Receiver) ->
    {ok, FileList} = file:list_dir(DirName),
    ExtendedFileList = lists:map(fun(FileName) ->
                                     filename:join([DirName, FileName])
                                 end, FileList),
    {Files, Dirs} = categorize(ExtendedFileList),
    lists:foreach(fun(Dir) ->
                          spawn(fun() -> process_dir(Dir, Receiver) end)
                  end, Dirs),
    lists:foreach(fun(File) ->
                          spawn(fun() -> process_file(File, Receiver) end)
                  end, Files).

process_file(FileName, Receiver) ->
    Size = get_size(FileName),
    Message = case file:open(FileName, [read]) of
                  {ok, File} ->
                      {LineCount, LineLengths} = process_lines(File),
                      FileType = get_filetype(FileName),
                      {file_processed, #file_info{name=FileName,
                                                  size=Size,
                                                  line_count=LineCount,
                                                  line_lengths=LineLengths,
                                                  filetype=FileType}};
                  {error, Error} ->
                      {error, {Error, FileName}}
              end,
    Receiver ! Message.

process_lines(File) ->
    do_process_lines(File, {0, []}).

do_process_lines(Device, {LineCount, LineLengths}) ->
    case io:get_line(Device, "") of
        eof -> file:close(Device), {LineCount, LineLengths};
        Line -> do_process_lines(Device,
                                 {LineCount + 1, [length(Line)|LineLengths]})
    end.

get_filetype(FileName) -> 
    case filename:extension(FileName) of
        [] -> "no filetype";
        FileType -> FileType
    end.

get_size(FileName) -> filelib:file_size(FileName).

categorize(FileNames) ->
    Files = [FileName || FileName <- FileNames,
                         not filelib:is_dir(FileName)],
    Dirs = [FileName || FileName <- FileNames,
                        filelib:is_dir(FileName)],
    {Files, Dirs}.

parser() ->
    cli:parser(
      "linerl",
      "[OPTION]...\n",
      "line counting utility\n",
      [{directiry, "-d, --dir",
        "directory to analyze"},
       {filetype, "-t, --filetype",
        "filetypes to search for",
        [optional_arg]}],
      [{version, "0.0.1\n" }]).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, _Parsed}, _P}) ->
    {ok, CurDir} = file:get_cwd(),
    process_dir(CurDir, self()),
    loop(#state{});
handle_parsed({{error, Err}, P}) ->
    cli:print_error_and_halt(Err, P).

