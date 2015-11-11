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

%% API
-export([init/1, insert/2]).

%%%===================================================================
%%% API
%%%===================================================================

init(Map) ->
    NumberOfFoams = round(?FOAM_INITIAL_GENERATION_SIZE *
        ?WIDTH_PER_WORKER * ?WIDTH_PER_WORKER),
    RandomFoams = [random_foam() || _ <- lists:seq(1, NumberOfFoams)],
    Foams = lists:usort(RandomFoams),
    lists:foldl(fun(Foam, Dict) -> foam:insert(Dict, Foam) end, Map, Foams).

insert(Map, Foam = #foam{coords = Coords}) ->
    dict:store(Coords, Foam, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_foam() ->
    #foam{coords = utils:random_coordinates(?WIDTH_PER_WORKER)}.