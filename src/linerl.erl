-module(linerl).
-export([main/1]).

main(Args) ->
    Parser = parser(),
    handle_parsed(cli:parse_args(Args, Parser)).

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
handle_parsed({{ok, {Opts, _Args}}, _P}) ->
    FileTypes = proplists:get_value(filetype, Opts),
    {match, _FileTypes2} = re:run(FileTypes, "\\w+", [global, {capture, [0], list}]),
    looper:start();
handle_parsed({{error, Err}, P}) ->
    cli:print_error(Err, P).

