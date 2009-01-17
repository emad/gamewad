%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(gamewad).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
set_mnesiaenv(PathFun) ->
    application:set_env(mnesia, dir,
                        PathFun(["priv", "mnesia", node()])),
    mnesia:create_schema([node()]),
    ok.

%% @spec start() -> ok
%% @doc Start the gamewad server.
start() ->
    gamewad_deps:ensure(),
    set_mnesiaenv(fun gamewad_deps:local_path/1),
    ensure_started(crypto),
    ensure_started(mnesia),
    ensure_started(egeoip),

    application:start(gamewad).

%% @spec stop() -> ok
%% @doc Stop the gamewad server.
stop() ->
    Res = application:stop(gamewad),
    application:stop(crypto),
    Res.
