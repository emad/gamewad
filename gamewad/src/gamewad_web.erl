%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for gamewad.

-module(gamewad_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).
-define(EPOCHDELTA, 62167219200).
-define(HMACKEY, "usearandomstringhere").

-compile([export_all]).
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
                "gamejson/" ++ GameSlug0 ->
                    GameSlug = list_to_binary(GameSlug0),
                    Data = gamewad_games:game_json(GameSlug),
                    SafeCountry = cpm_country(Req:get(peer)),
                    {ok, Guid} = get_guid(Req),
                    Data1 = {struct, [{game, Data},
                                      {is_favorite,
                                       gamewad_users:is_favorite(Guid,
                                                                 GameSlug)},
                                      {use_local, SafeCountry}]},
                    Req:ok({<<"application/json">>,
                            mochijson2:encode(Data1)});
                "random_games/" ++ Count ->
                    C = list_to_integer(Count),
                    Data = gamewad_games:random_games(C),
                    Req:ok({<<"application/json">>,
                            [{"Cache-Control", "no-cache"}, {"Expires", "Mon, 01 Jan 1009 01:00:00 GMT"}],
                            mochijson2:encode(Data)});
                "user/favorites/get" ++ _ ->
                    {ok, Guid} = get_guid(Req),
                    GameList = gamewad_users:get_favorites(Guid),
                    Games = 
                             [gamewad_games:game_json(G)
                              || G <- GameList],
                    Req:ok({<<"application/json">>,
                            [{"Cache-Control", "no-cache"}, {"Expires", "Mon, 01 Jan 1009 01:00:00 GMT"}],
                            mochijson2:encode(Games)});
                _ ->
                    {ok, Guid} = get_guid(Req),
                     H = [mochiweb_cookies:cookie("gamewad_user",
                                                  guid_cookie(Guid),
                                                  [{path, "/"},
                                                   {max_age, 315360000}])],
                    Req:serve_file(Path, DocRoot, H)
            end;
        'POST' ->
            case Path of
                "user/favorites/add/" ++ GameSlug ->
                    {ok, Guid} = get_guid(Req),
                    ok = gamewad_users:add_favorite(Guid,
                                                    list_to_binary(GameSlug)),
                    Req:ok({<<"application/json">>,
                            <<"{\"status\":\"ok\"}">>});
                "user/favorites/delete/" ++ GameSlug ->
                    {ok, Guid} = get_guid(Req),
                    ok = gamewad_users:delete_favorite(Guid,
                                                    list_to_binary(GameSlug)),
                    Req:ok({<<"application/json">>,
                            <<"{\"status\":\"ok\"}">>});
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


get_guid(Req) ->
    get_guid(Req, "gamewad_user").

get_guid(Req, CookieName) ->
    case Req:get_cookie_value(CookieName) of
        undefined ->
            Guid = generate_guid(),
            {ok, Guid};
        GuidCookie ->
            case valid_guid_cookie(GuidCookie) of
                {ok, Guid} ->
                    {ok, Guid};
                invalid ->
                    {ok, generate_guid()}
            end
    end.



timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time())
        - ?EPOCHDELTA.

guid_cookie(Guid) ->
    TS = integer_to_list(timestamp()),
    N = mochicode:url64_encode(crypto:rand_bytes(6)),
    Data = Guid ++ TS ++ N,
    HMAC = mochicode:url64_encode(crypto:sha_mac(?HMACKEY, Data)),
    [Guid, $&, TS, $&, N, $&, HMAC].

valid_guid_cookie(GuidCookie) ->
    [Guid, TS, N, HMAC] = string:tokens(GuidCookie, "&"),
    Data = Guid ++ TS ++ N,
    case mochicode:url64_encode(crypto:sha_mac(?HMACKEY, Data)) of
        HMAC ->
            {ok, Guid};
        _ ->
            invalid
    end.

generate_guid() ->
    generate_id(<<"0123456789abcdef">>, <<"fedcba9876543210">>).

generate_id(Key, IoVec) ->
    {Mega, S, Micro} = now(),
    I = trunc(Mega*1.0e+9 + S*1.0e+3 + Micro*1.0e-3),
    Bytes = crypto:rand_bytes(8),
    Data = <<Bytes:64/bits, I:64/integer>>,
    mochihex:to_hex(crypto:aes_cbc_128_encrypt(Key, IoVec, Data)).
