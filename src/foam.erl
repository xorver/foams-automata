%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(foam).
-author("Tomasz Lichon").

-include("config.hrl").
-include("foam.hrl").
-include("algae.hrl").

%% API
-export([init/1, insert/2, move_all/1, reproduce_all/1, starve_all/1, remove_dead/1]).

%%%===================================================================
%%% API
%%%===================================================================

init(Map) ->
    NumberOfFoams = round(?FOAM_INITIAL_GENERATION_SIZE *
        ?WIDTH_PER_WORKER * ?WIDTH_PER_WORKER),
    RandomFoams = [random_foam() || _ <- lists:seq(1, NumberOfFoams)],
    Foams = lists:usort(RandomFoams),
    lists:foldl(fun(Foam, Dict) -> foam:insert(Dict, Foam) end, Map, Foams).

insert(Map, Foam = #foam{coords = Coords, energy = Energy}) ->
    dict:update(Coords,
        fun(#algae{energy = OldEnergy}) ->
            Foam#foam{energy = OldEnergy + Energy}
        end, Foam, Map).

move_all(Map) ->
    dict:fold(
        fun
            (_, #foam{} = V, D) ->
                move(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

move(Map, Foam = #foam{coords = Coords}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = dict:erase(Coords, Map),
            insert(Map2, Foam#foam{coords = NewCoord})
    end.

reproduce_all(Map) ->
    dict:fold(
        fun
            (_, V = #foam{energy = Energy}, D) when Energy >= ?FOAM_REPRODUCTION_LIMIT ->
                reproduce(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

reproduce(Map, Foam = #foam{coords = Coords, energy = Energy}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = dict:store(Coords, Foam#foam{energy = Energy/2}, Map),
            insert(Map2, Foam#foam{coords = NewCoord, energy = Energy/2})
    end.

starve_all(Map) ->
    dict:fold(
        fun
            (_, #foam{} = V, D) ->
                starve(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

starve(Map, Foam = #foam{coords = Coords, energy = Energy}) ->
    dict:store(Coords, Foam#foam{energy = Energy - ?FOAM_STARVATION_RATE}, Map).

remove_dead(Map) ->
    dict:filter(
        fun
            (_, #foam{energy = Energy}) when Energy =< 0 ->
                false;
            (_, _) ->
                true
        end, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_foam() ->
    #foam{coords = utils:random_coordinates(?WIDTH_PER_WORKER)}.

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
        _ ->
            true
    end.
