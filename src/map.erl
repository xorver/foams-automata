%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(map).
-author("Tomasz Lichon").

-include("config.hrl").
-include("algae.hrl").
-include("foam.hrl").

%% API
-export([dump/2]).

%%%===================================================================
%%% API
%%%===================================================================
dump(Map, _Iteration) ->
    case ?PRINT_OUTPUT of
        true ->
            Balance = dict:fold(
                fun
                    (_, #foam{}, {FoamNum, AlgaeNum}) ->
                        {FoamNum + 1, AlgaeNum};
                    (_, #algae{}, {FoamNum, AlgaeNum}) ->
                        {FoamNum, AlgaeNum + 1}
                end, {0, 0}, Map),
            io:format(" ~p ", [Balance]);
%%             io:format("~p ~n", [dict:to_list(Map)]);
        false ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================