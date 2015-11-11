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
-export([init/1, insert/2]).

%%%===================================================================
%%% API
%%%===================================================================
init(Map) ->
    NumberOfAlgaes = round(?ALGAE_INITIAL_GENERATION_SIZE *
        ?WIDTH_PER_WORKER * ?WIDTH_PER_WORKER),
    RandomAlgaes = [random_algae() || _ <- lists:seq(1, NumberOfAlgaes)],
    Algaes = lists:usort(RandomAlgaes),
    lists:foldl(fun(Algae, Dict) -> algae:insert(Dict, Algae) end, Map, Algaes).

insert(Map, Algae = #algae{coords = Coords, energy = Energy}) ->
    dict:update(Coords,
        fun
            (Old = #algae{energy = OldEnergy}) ->
                Old#algae{energy = OldEnergy + Energy};
            (Old = #foam{energy = OldEnergy}) ->
                Old#foam{energy = OldEnergy + Energy}
        end, Algae, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_algae() ->
    #algae{coords = utils:random_coordinates(?WIDTH_PER_WORKER)}.