%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, 1).

-define(PRINT_OUTPUT, false).

-define(WORKERS_WIDTH, 1).
-define(WIDTH_PER_WORKER, 100).
-define(STEPS, 10000).

-define(FORAM_INITIAL_GENERATION_SIZE, 0.2).
-define(FORAM_REPRODUCTION_LIMIT, 20).
-define(FORAM_INITIAL_ENERGY, 15).
-define(FORAM_STARVATION_RATE, 1).

-define(ALGAE_INITIAL_GENERATION_SIZE, 0.1).
-define(ALGAE_REPRODUCTION_LIMIT, 10).
-define(ALGAE_INITIAL_ENERGY, 5).
-define(ALGAE_GROWTH_RATE, 2).
-define(ALGAE_SPAWN_SIZE, 0.01).

-define(VALID_MOVES, [{-1, 1}, {-1, 0}, {-1, -1}, {0, 1}, {0, -1}, {1, 1}, {1, 0}, {1, -1}]).

-endif.