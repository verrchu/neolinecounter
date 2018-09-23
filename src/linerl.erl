-module(linerl).
-export([main/1]).

main(Args) ->
    Parser = parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

parser() ->
    cli:parser(
      "linerl",
      "[OPTION]...",
      "line counting utility\n",
      [{directiry, "-d, --dir", "directory to analyze"},
       {filetype, "-t, --filetype", "filetypes to search for", [optional_arg]}],
      [{version, "0.0.1\n" }, {pos_args, 1}]).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, {Opts, _Args}}, _P}) ->
    FileTypes = parse_filetype(Opts),
    looper:start(FileTypes);
handle_parsed({{error, Err}, P}) ->
    cli:print_error(Err, P).

parse_filetype(Opts) ->
    case proplists:get_value(filetype, Opts) of
        undefined -> [];
        FT ->
            {match, FT2} =
                re:run(FT, "\\w+", [global, {capture, [0], list}]),
            FT3 = lists:map(fun hd/1, FT2),
            FT4 = lists:map(fun(X) -> [$. | X] end, FT3),
            FT4
    end.

