-module(linerl).
-export([main/1]).

main(Args) ->
    Parser = parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

parser() ->
    cli:parser(
      "linerl",
      "[OPTION]... [DIR]",
      "line counting utility\n",
      [{filetype, "-t, --filetype", "filetypes to search for", [optional_arg]}],
      [{version, "0.0.1\n" }]).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, {Opts, Args}}, _P}) ->
    FileTypes = parse_filetype(Opts),
    Dir = parse_dir(Args),
    looper:start(Dir, FileTypes);
handle_parsed({{error, Err}, P}) ->
    cli:print_error(Err, P).

parse_filetype(Opts) ->
    case proplists:get_value(filetype, Opts) of
        undefined -> [];
        FT ->
            {match, FT2} = re:run(FT, "\\w+", [global, {capture, [0], list}]),
            FT3 = lists:map(fun hd/1, FT2),
            FT4 = lists:map(fun(X) -> [$. | X] end, FT3),
            FT4
    end.

parse_dir([]) ->
    {ok, Dir} = file:get_cwd(),
    Dir;
parse_dir([Dir|_]) ->
    {ok, CurDir} = file:get_cwd(),
    filename:join(CurDir, Dir).
