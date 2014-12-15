-module(efene_rebar3_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%-include_lib("rebar3/include/rebar.hrl").

-define(PROVIDER, efene).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar efene file.fn"}, % How to use the plugin
            {opts, []},                  % list of options understood by the plugin
            {short_desc, "efene rebar3 plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun compile_sources/1, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

compile(Path, DestPath) ->
    io:format("Compiling ~s~n", [Path]),
    efene:compile(Path, DestPath).

compile_sources(App) ->
    Path = filename:join(rebar_app_info:dir(App), "src"),
    DestPath = filename:join(rebar_app_info:dir(App), "ebin"),
    ok = filelib:ensure_dir(filename:join(DestPath, "a")),
    Mods = find_source_files(Path),
    lists:foreach(fun (ModPath) -> compile(ModPath, DestPath) end, Mods),
    ok.

find_source_files(Path) ->
    [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.fn", Path)].
