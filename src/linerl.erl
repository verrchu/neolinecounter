-module(linerl).
-export([main/1]).

-record(file_info, {name, size, line_count, line_lengths, filetype}).
-record(filetype_info, {file_count, line_count}).
-record(state, {info = #{}}).

-define(LOOP_TIMEOUT, 1000).

main([]) ->
    {ok, CurDir} = file:get_cwd(),
    process_dir(CurDir, self()),
    loop(#state{}).

loop(#state{info = Info}) ->
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
            loop(#state{info = NewInfo})
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
    {LineCount, LineLengths} = process_lines(FileName),
    FileType = get_filetype(FileName),
    Receiver ! {file_processed, #file_info{name=FileName,
                                           size=Size,
                                           line_count=LineCount,
                                           line_lengths=LineLengths,
                                           filetype=FileType}}.

process_lines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    do_process_lines(Device, {0, []}).

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
