%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the gamewad application.

-module(gamewad_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for gamewad.
start(_Type, _StartArgs) ->
    gamewad_deps:ensure(),
    gamewad_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for gamewad.
stop(_State) ->
    ok.
