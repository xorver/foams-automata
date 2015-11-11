%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(foram).
-author("Tomasz Lichon").

-include("config.hrl").
-include("foram.hrl").
-include("algae.hrl").

%% API
-export([init/1, insert/2, move_all/1, reproduce_all/1, starve_all/1, remove_dead/1]).

%%%===================================================================
%%% API
%%%===================================================================

init(Map) ->
    NumberOfForams = round(?FORAM_INITIAL_GENERATION_SIZE *
        ?WIDTH_PER_WORKER * ?WIDTH_PER_WORKER),
    RandomForams = [random_foram() || _ <- lists:seq(1, NumberOfForams)],
    Forams = lists:usort(RandomForams),
    lists:foldl(fun(Foram, Dict) -> foram:insert(Dict, Foram) end, Map, Forams).

insert(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    dict:update(Coords,
        fun(#algae{energy = OldEnergy}) ->
            Foram#foram{energy = OldEnergy + Energy}
        end, Foram, Map).

move_all(Map) ->
    dict:fold(
        fun
            (_, #foram{} = V, D) ->
                move(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

move(Map, Foram = #foram{coords = Coords}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = dict:erase(Coords, Map),
            insert(Map2, Foram#foram{coords = NewCoord})
    end.

reproduce_all(Map) ->
    dict:fold(
        fun
            (_, V = #foram{energy = Energy}, D) when Energy >= ?FORAM_REPRODUCTION_LIMIT ->
                reproduce(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

reproduce(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = dict:store(Coords, Foram#foram{energy = Energy/2}, Map),
            insert(Map2, Foram#foram{coords = NewCoord, energy = Energy/2})
    end.

starve_all(Map) ->
    dict:fold(
        fun
            (_, #foram{} = V, D) ->
                starve(D, V);
            (_, _, D) ->
                D
        end, Map, Map
    ).

starve(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    dict:store(Coords, Foram#foram{energy = Energy - ?FORAM_STARVATION_RATE}, Map).

remove_dead(Map) ->
    dict:filter(
        fun
            (_, #foram{energy = Energy}) when Energy =< 0 ->
                false;
            (_, _) ->
                true
        end, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_foram() ->
    #foram{coords = utils:random_coordinates(?WIDTH_PER_WORKER)}.

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
        {ok, #foram{}} ->
            false;
        _ ->
            true
    end.
