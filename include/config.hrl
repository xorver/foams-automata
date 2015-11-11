%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, 1).

-define(PRINT_OUTPUT, true).

-define(WORKERS_WIDTH, 1).
-define(WIDTH_PER_WORKER, 10).
-define(STEPS, 50).

-define(FORAM_INITIAL_GENERATION_SIZE, 0.1).
-define(FORAM_REPRODUCTION_LIMIT, 10).
-define(FORAM_INITIAL_ENERGY, 8).
-define(FORAM_STARVATION_RATE, 1).

-define(ALGAE_INITIAL_GENERATION_SIZE, 0.1).
-define(ALGAE_REPRODUCTION_LIMIT, 10).
-define(ALGAE_INITIAL_ENERGY, 5).
-define(ALGAE_GROWTH_RATE, 2).

-define(VALID_MOVES, [{-1, 1}, {-1, 0}, {-1, -1}, {0, 1}, {0, -1}, {1, 1}, {1, 0}, {1, -1}]).

-endif.