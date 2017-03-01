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

    {RawOpts, _} = rebar_state:command_parsed_args(State) ,
    Format = proplists:get_value(format, RawOpts, "beam"),
    FirstFiles = [],
    SourceExt = ".fn",
    TargetExt = "." ++ Format,

    [begin
         Opts = rebar_app_info:opts(AppInfo),
         TargetDir = rebar_app_info:ebin_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),

         CompileFun = fun(Source, Target, Opts1) ->
                              ErlOpts = rebar_opts:erl_opts(Opts1),
                              compile_source(State, ErlOpts, Source, Target)
                      end,

         rebar_base_compiler:run(Opts, FirstFiles, SourceDir, SourceExt,
                                 TargetDir, TargetExt, CompileFun, [])
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

compile("rawlex", Path, _DestPath, _ErlOpts) ->
    rebar_api:info("~P", [efene:to_raw_lex(Path), 1000]);
compile("lex", Path, _DestPath, _ErlOpts) ->
    rebar_api:info("~P", [efene:to_lex(Path), 1000]);
compile("ast", Path, _DestPath, _ErlOpts) ->
    rebar_api:info("~P", [efene:to_ast(Path), 1000]);
compile("mod", Path, _DestPath, _ErlOpts) ->
    rebar_api:info("~P", [efene:to_mod(Path), 1000]);
compile("erl", Path, DestDirPath, _ErlOpts) ->
    BaseName = filename:basename(Path, ".fn"),
    DestName = BaseName ++ ".erl",
    DestPath = filename:join(DestDirPath, DestName),
    rebar_api:info("writing to ~s", [DestPath]),
    file:write_file(DestPath, efene:to_erl(Path)),
    ok;
compile("erlast", Path, _DestPath, _ErlOpts) ->
    Data = case efene:to_erl_ast(Path) of
               {ok, {Ast, _State}} -> Ast;
               Other -> Other
           end,
    rebar_api:info("~P", [Data, 1000]);
compile("beam", Path, DestPath, ErlOpts) ->
    case efene:compile(Path, DestPath, ErlOpts) of
        {error, _}=Error ->
            FmtErrors = [fn_error:normalize(Error)],
            {error, FmtErrors, []};
        {error, Errors, Warnings} ->
            FmtErrors = [fn_error:normalize(Error) || Error  <- Errors],
            FmtWarnings = [fn_error:normalize(Warn) || Warn  <- Warnings],
            {error, FmtErrors, FmtWarnings};
        {ok, CompileInfo} ->
            Warnings = proplists:get_value(warnings, CompileInfo, []),
            FmtWarnings = [fn_error:normalize(Warn) || Warn  <- Warnings],
            {ok, FmtWarnings};
        Other ->
            {error, [io_lib:format("Unknown result: ~p", [Other])], []}
    end;
compile(Format, _Path, _DestPath, _ErlOpts) ->
    rebar_api:error("Invalid format: ~s", [Format]).

compile_source(State, ErlOpts, Source, DestPath) ->
    ok = filelib:ensure_dir(DestPath),
    {RawOpts, _} = rebar_state:command_parsed_args(State) ,
    Format = proplists:get_value(format, RawOpts, "beam"),
    rebar_api:info("Compiling ~s", [Source]),
    compile(Format, Source, filename:dirname(DestPath), ErlOpts).

help(format) -> "format to compile code to, one of rawlex, lex, ast, mod, erlast, erl, beam";
help(file) -> "file to compile, if omited all files in the project are compiled".
