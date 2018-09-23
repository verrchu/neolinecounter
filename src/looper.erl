-module(looper).

-export([start/2]).

-record(file_info, {name, size, line_count, line_lengths, filetype}).
-record(filetype_info, {file_count, line_count}).
-record(state, {info = #{}, filetypes = all}).

-define(LOOP_TIMEOUT, 1000).

start(Dir, FileTypes) ->
    process_dir(Dir, FileTypes, self()),
    loop(#state{}).

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
        {{error, Error}, {file, FileName}} ->
            io:format("error |~p| while processing file:~n~p~n", [Error, FileName]),
            loop(State);
        {{error, Error}, {dir, DirName}} ->
            io:format("error |~p| while processing dir:~n~p~n", [Error, DirName]),
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

process_dir(DirName, FileTypes, Receiver) ->
    case file:list_dir(DirName) of
        {ok, FileList} ->
            ExtendedFileList = lists:map(fun(FileName) ->
                                             filename:join([DirName, FileName])
                                         end, FileList),
            {Files, Dirs} = categorize(ExtendedFileList),
            lists:foreach(fun(Dir) ->
                                  spawn(fun() -> process_dir(Dir, FileTypes, Receiver) end)
                          end, Dirs),
            lists:foreach(fun(File) ->
                                  spawn(fun() -> process_file(File, FileTypes, Receiver) end)
                          end, Files);
        {error, Error} ->
            Receiver ! {{error, Error}, {dir, DirName}}
    end.

process_file(FileName, FileTypes, Receiver) ->
    FileType = get_filetype(FileName),
    case lists:member(FileType, FileTypes) orelse FileTypes == [] of
        false -> void;
        true ->
            Message = case file:open(FileName, [read]) of
                          {ok, File} ->
                              {LineCount, LineLengths} = process_lines(File),
                              Size = get_size(FileName),
                              {file_processed, #file_info{name=FileName,
                                                          size=Size,
                                                          line_count=LineCount,
                                                          line_lengths=LineLengths,
                                                          filetype=FileType}};
                          {error, Error} ->
                              {{error, Error}, {file, FileName}}
                      end,
            Receiver ! Message
    end.

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

