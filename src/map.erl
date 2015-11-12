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
-include("foram.hrl").

%% API
-export([dump/2]).

%%%===================================================================
%%% API
%%%===================================================================
dump(Map, Iteration) ->
    case ?PRINT_OUTPUT of
        true ->
            Balance = get_balance(Map),
            io:format(" ~p ", [Balance]);
        false ->
            dump_to_file(Map, Iteration)
    end.

get_balance(Map) ->
    dict:fold(fun
        (_, #foram{}, {ForamNum, AlgaeNum}) ->
            {ForamNum + 1, AlgaeNum};
        (_, #algae{}, {ForamNum, AlgaeNum}) ->
            {ForamNum, AlgaeNum + 1}
    end, {0, 0}, Map).

dump_to_file(Map, Iteration) ->
    Filename = filename:join("../../data", integer_to_list(Iteration) ++ ".txt"),
    {ForamNum, AlgaeNum} = get_balance(Map),
    Header = <<"Step: ", (integer_to_binary(Iteration))/binary,
    ", Forams: ", (integer_to_binary(ForamNum))/binary,
    ", Algae: ", (integer_to_binary(AlgaeNum))/binary, "\n\n">>,
    file:write_file(Filename, Header, [write]),
    for(0, ?WORKERS_WIDTH - 1, fun(Row) ->
        for(0, ?WIDTH_PER_WORKER - 1, fun(Y) ->
            NewLine = lists:foldl(fun(Column, Line) ->
                NewLinePart = lists:foldl(fun(X, LinePart) ->
                    case dict:find({Column * ?WORKERS_WIDTH + X, Row * ?WORKERS_WIDTH + Y}, Map) of
                        {ok, #foram{energy = E}} ->
                            <<LinePart/binary, (red(E))/binary, "\t">>;
                        {ok, #algae{energy = E}} ->
                            <<LinePart/binary, (green(E))/binary, "\t">>;
                        _ -> <<LinePart/binary, ".\t">>
                    end
                end, <<>>, lists:seq(0, ?WIDTH_PER_WORKER - 1)),
                <<Line/binary, NewLinePart/binary>>
            end, <<>>, lists:seq(0, ?WORKERS_WIDTH - 1)),
            file:write_file(Filename, <<NewLine/binary, "\n">>, [append])
        end)
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

red(Value) ->
    <<"\e[31m", (integer_to_binary(round(Value)))/binary, "\e[0m">>.

green(Value) ->
    <<"\e[32m", (integer_to_binary(round(Value)))/binary, "\e[0m">>.

for(I, N, _Fun) when I > N -> ok;
for(I, N, Fun) ->
    Fun(I),
    for(I + 1, N, Fun).