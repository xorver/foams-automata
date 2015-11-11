%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(algae).
-author("Tomasz Lichon").

-include("config.hrl").
-include("algae.hrl").
-include("foam.hrl").

%% API
-export([init/1, insert/2, grow_all/1, reproduce_all/1]).

%%%===================================================================
%%% API
%%%===================================================================
init(Map) ->
    NumberOfAlgaes = round(?ALGAE_INITIAL_GENERATION_SIZE *
        ?WIDTH_PER_WORKER * ?WIDTH_PER_WORKER),
    RandomAlgaes = [random_algae() || _ <- lists:seq(1, NumberOfAlgaes)],
    Algaes = lists:usort(RandomAlgaes),
    lists:foldl(fun(Algae, Dict) -> algae:insert(Dict, Algae) end, Map, Algaes).

insert(Map, Algae = #algae{coords = Coords}) ->
    dict:update(Coords,
        fun
            (#algae{}) ->
                Algae;
            (Old = #foam{}) ->
                Old
        end, Algae, Map).

grow_all(Map) ->
    dict:fold(
        fun
            (_, #algae{} = V, D) ->
                grow(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

grow(Map, Foam = #algae{energy = Energy}) ->
    insert(Map, Foam#algae{energy = Energy + ?ALGAE_GROWTH_RATE}).

reproduce_all(Map) ->
    dict:fold(
        fun
            (_, V = #algae{energy = Energy}, D) when Energy >= ?ALGAE_REPRODUCTION_LIMIT ->
                reproduce(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

reproduce(Map, Algae = #algae{coords = Coords, energy = Energy}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = dict:store(Coords, Algae#algae{energy = Energy/2}, Map),
            insert(Map2, Algae#algae{coords = NewCoord, energy = Energy/2})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_algae() ->
    #algae{coords = utils:random_coordinates(?WIDTH_PER_WORKER)}.

random_valid_move(_, []) ->
    undefined;
random_valid_move(Map, [Move | Rest]) ->
    case is_valid_move(Map, Move) of
        true ->
            Move;
        false ->
            random_valid_move(Map, Rest)
    end;
random_valid_move(Map, {X,Y}) ->
    DeltasOfMoves = utils:random_shuffle(?VALID_MOVES),
    Moves = [{X + DX, Y + DY} || {DX, DY} <- DeltasOfMoves],
    random_valid_move(Map, Moves).

is_valid_move(_, {X, _}) when X < 0 orelse X >= ?WIDTH_PER_WORKER ->
    false;
is_valid_move(_, {_, Y}) when Y < 0 orelse Y >= ?WIDTH_PER_WORKER ->
    false;
is_valid_move(Map, Coords)->
    case dict:find(Coords, Map) of
        {ok, #foam{}} ->
            false;
        {ok, #algae{}} ->
            false;
        _ ->
            true
    end.