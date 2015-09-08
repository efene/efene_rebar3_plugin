-module(rebar3_efene_compile).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%-include_lib("rebar3/include/rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [{default, install_deps}, {default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {namespace, efene},
            {bare, false},
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar efene compile"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [{format, undefined, "format", string, help(format)},
                    {file, undefined, "file", string, help(file)}]},
            {short_desc, "efene rebar3 plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    CompilePaths = compile_paths(State),
    lists:foreach(fun ({Path, DestPath, ErlOpts}) ->
                          compile_sources(State, ErlOpts, Path, DestPath)
                  end, CompilePaths),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

compile_paths(State) ->
    Path = filename:join(rebar_state:dir(State), "src"),
    SrcPathExists = filelib:is_dir(Path),

    % if the src folder exists, we assume is not a release, so we compile
    % the src folder only
    if SrcPathExists ->
           DestPath = filename:join(rebar_state:dir(State), "ebin"),
           ErlOpts = rebar_state:get(State, erl_opts, []),
           [{Path, DestPath, ErlOpts}];
       true ->
           Apps = rebar_state:project_apps(State),
           lists:map(fun (App) ->
                             AppOpts = rebar_app_info:opts(App),
                             ErlOpts = rebar_opts:erl_opts(AppOpts),
                             AppPath = filename:join(rebar_app_info:dir(App), "src"),
                             AppDestPath = filename:join(rebar_app_info:dir(App), "ebin"),
                             {AppPath, AppDestPath, ErlOpts}
                     end, Apps)
    end.

compile("rawlex", Path, _DestPath, _ErlOpts) ->
    io:format("~P~n", [efene:to_raw_lex(Path), 1000]);
compile("lex", Path, _DestPath, _ErlOpts) ->
    io:format("~P~n", [efene:to_lex(Path), 1000]);
compile("ast", Path, _DestPath, _ErlOpts) ->
    io:format("~P~n", [efene:to_ast(Path), 1000]);
compile("mod", Path, _DestPath, _ErlOpts) ->
    io:format("~P~n", [efene:to_mod(Path), 1000]);
compile("erl", Path, _DestPath, _ErlOpts) ->
    io:format("~s~n", [efene:to_erl(Path)]);
compile("erlast", Path, _DestPath, _ErlOpts) ->
    Data = case efene:to_erl_ast(Path) of
               {ok, {Ast, _State}} -> Ast;
               Other -> Other
           end,
    io:format("~P~n", [Data, 1000]);
compile("beam", Path, DestPath, ErlOpts) ->
    case efene:compile(Path, DestPath, ErlOpts) of
        {error, _}=Error ->
            efene:print_errors([Error], "errors");
        {error, Errors, Warnings} ->
            efene:print_errors(Errors, "errors"),
            efene:print_errors(Warnings, "warnings"),
            ok;
        {ok, CompileInfo} ->
            efene:print_errors(proplists:get_value(warnings, CompileInfo, []), "warnings"),
            ok;
        Other ->
            io:format("unknown result: ~p~n", [Other]),
            Other
    end;
compile(Format, _Path, _DestPath, _ErlOpts) ->
    io:format("Invalid format: ~s~n", [Format]).

compile_sources(State, ErlOpts, Path, DestPath) ->
    ok = filelib:ensure_dir(filename:join(DestPath, "a")),
    Mods = find_source_files(Path),
    {RawOpts, _} = rebar_state:command_parsed_args(State) ,
    Format = proplists:get_value(format, RawOpts, "beam"),
    case proplists:lookup(file, RawOpts) of
        none ->
            lists:foreach(fun (ModPath) ->
                              io:format("Compiling ~s~n", [ModPath]),
                              compile(Format, ModPath, DestPath, ErlOpts)
                          end, Mods);
        {file, ModPath} ->
            compile(Format, ModPath, DestPath, ErlOpts)
    end,
    ok.

find_source_files(Path) ->
    [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.fn", Path)].

help(format) -> "format to compile code to, one of rawlex, lex, ast, mod, erlast, erl, beam";
help(file) -> "file to compile, if omited all files in the project are compiled".
