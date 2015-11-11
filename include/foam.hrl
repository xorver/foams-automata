%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(FOAM_HRL).
-define(FOAM_HRL, 1).

-include("config.hrl").

-record(foam, {
    coords :: {non_neg_integer(), non_neg_integer()},
    energy = ?FOAM_INITIAL_ENERGY :: integer()
}).

-endif.