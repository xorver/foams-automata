%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ALGAE_HRL).
-define(ALGAE_HRL, 1).

-include("config.hrl").

-record(algae, {
    coords :: {non_neg_integer(), non_neg_integer()},
    energy = ?ALGAE_INITIAL_ENERGY :: non_neg_integer()
}).

-endif.