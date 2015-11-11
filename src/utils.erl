%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(utils).
-author("Tomasz Lichon").

%% API
-export([for/3, random_coordinates/1, random_shuffle/1]).

%%%===================================================================
%%% API
%%%===================================================================

for(I, N, _Fun) when I > N -> ok;
for(I, N, Fun) ->
    Fun(I),
    for(I + 1, N, Fun).

random_coordinates(Max) ->
    {random:uniform(Max) - 1, random:uniform(Max) - 1}.

-spec random_shuffle(List :: list()) -> NewList :: list().
random_shuffle(List) ->
    From = 0,
    To = length(List) + 1,
    [X || {_, X} <- lists:sort([{crypto:rand_uniform(From, To), N} || N <- List])].

%%%===================================================================
%%% Internal functions
%%%===================================================================