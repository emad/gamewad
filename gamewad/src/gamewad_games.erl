-module(gamewad_games).


-compile([export_all]).

-record(gamejson, {gameslug, json}).
-record(keyvalues, {k,v}).

create_tables() ->
    {atomic, ok} = mnesia:create_table(gamejson,
                             [{attributes, record_info(fields, gamejson)},
                              {disc_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(keyvalues,
                             [{attributes, record_info(fields, keyvalues)},
                              {disc_copies, [node()]}]).

game_json(Slug) ->
    case mnesia:dirty_read(gamejson, Slug) of
        [#gamejson{json=G}] ->
            G;
        [] ->
            null
    end.

random_games(N) ->
    random_games(N, [<<"thumbnail_url">>, <<"slug">>]).

random_games(N, Keys) ->
    S = mnesia:table_info(gamejson, size),
    {ok, Offsets} = random_list(0, S, N),
    [begin
         [#keyvalues{v=Slug}] = mnesia:dirty_read({keyvalues, O}),
         [#gamejson{json=GD}] = mnesia:dirty_read({gamejson, Slug}),
         filterkeys(GD, Keys)
     end || O <- Offsets].

random_list(Min, Max, N) when N > (Max-Min) ->
    {error, invalid_range};
random_list(Min, Max, N) ->
    F = fun() -> crypto:rand_uniform(Min, Max) end,
    {ok, random_list1(F, N, sets:new())}.

random_list1(F, N, Acc) ->
    case sets:size(Acc) of
        N ->
            sets:to_list(Acc);
        _ ->
            AccOut = sets:add_element(F(), Acc),
            random_list1(F, N, AccOut)
    end.

filterkeys({struct, Data}, Keys) ->
    {struct, [{K, proplists:get_value(K, Data, <<"">>)} || K <- Keys]}.


load_games_from_file(File) ->
    {ok, D} = file:read_file(File),
    {struct, Data} = mochijson2:decode(D),
    Games = proplists:get_value(<<"games">>, Data),
    lists:foldl(fun write_game/2, 0, Games).


write_game({struct, GameData} = Game, Acc) ->
    Slug = proplists:get_value(<<"slug">>, GameData),
    %%Json = mochijson2:encode(Game),
    mnesia:dirty_write(#gamejson{gameslug=Slug,
                                 json=Game}),
    mnesia:dirty_write(#keyvalues{k=Acc, v=Slug}),
    Acc+1.
    
