-module(gamewad_users).



-export([create_tables/0]).
-export([add_favorite/2, delete_favorite/2, get_favorites/1, is_favorite/2]).

-record(favorite, {guid, gameslug}).

create_tables() ->
    {atomic, ok} = mnesia:create_table(favorite,
                             [{attributes, record_info(fields, favorite)},
                              {disc_copies, [node()]},
                              {type, bag}]),
    ok.

add_favorite(Guid, GameSlug) ->
    ok = mnesia:dirty_write(#favorite{guid=Guid, gameslug=GameSlug}),
    ok.

delete_favorite(Guid, GameSlug) ->
    ok = mnesia:dirty_delete_object(#favorite{guid=Guid, gameslug=GameSlug}).

get_favorites(Guid) ->
    [GameSlug || #favorite{gameslug=GameSlug} <- 
                     mnesia:dirty_read({favorite, Guid})].

is_favorite(Guid, GameSlug) ->
    case mnesia:dirty_match_object(#favorite{guid=Guid, gameslug=GameSlug}) of
        [] ->
            false;
        _ ->
            true
    end.
