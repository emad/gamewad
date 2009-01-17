%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for gamewad.

-module(gamewad_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

cpm_country(Peer) ->
    case egeoip:lookup(Peer) of
        {ok, G} ->
            lists:member(egeoip:get(G, country_code),
                         ["US", "GB", "NL", "NO", "DK", "ZA", "CA"]);
        _ ->
            false
    end.

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "gamejson/" ++ GameSlug ->
                    Data = gamewad_games:game_json(list_to_binary(GameSlug)),
                    SafeCountry = cpm_country(Req:get(peer)),
                    Data1 = {struct, [{game, Data}, {use_local, SafeCountry}]},
                    Req:ok({<<"application/json">>, 
                            mochijson2:encode(Data1)});
                "random_games/" ++ Count ->
                    C = list_to_integer(Count),
                    Data = gamewad_games:random_games(C),
                    Req:ok({<<"application/json">>, 
                            mochijson2:encode(Data)});
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
