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
    DepsPaths = rebar_state:code_paths(State, all_deps),
    %PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    %rebar_utils:remove_from_code_path(PluginDepsPaths),
    code:add_pathsa(DepsPaths),

    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),

         CompileFun = fun(Source, Opts1) ->
                              ErlOpts = rebar_opts:erl_opts(Opts1),
                              compile_source(State, ErlOpts, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], SourceDir, ".fn", OutDir, ".beam", CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

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

compile_source(State, ErlOpts, Source, DestPath) ->
    NewDestPath = filename:join(DestPath, "ebin"),
    ok = filelib:ensure_dir(filename:join(NewDestPath, "a")),
    {RawOpts, _} = rebar_state:command_parsed_args(State) ,
    Format = proplists:get_value(format, RawOpts, "beam"),
    io:format("Compiling ~s~n", [Source]),
    compile(Format, Source, NewDestPath, ErlOpts),
    ok.

help(format) -> "format to compile code to, one of rawlex, lex, ast, mod, erlast, erl, beam";
help(file) -> "file to compile, if omited all files in the project are compiled".
