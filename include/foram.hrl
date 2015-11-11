%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(FORAM_HRL).
-define(FORAM_HRL, 1).

-include("config.hrl").

-record(foram, {
    coords :: {non_neg_integer(), non_neg_integer()},
    energy = ?FORAM_INITIAL_ENERGY :: integer()
}).

-endif.