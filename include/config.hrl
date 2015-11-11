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
-define(STEPS, 10).

-define(FOAM_INITIAL_GENERATION_SIZE, 0.1).
-define(FOAM_INITIAL_ENERGY, 5).

-define(ALGAE_INITIAL_GENERATION_SIZE, 0.1).
-define(ALGAE_GENERATION_PROBABILITY, 0.2).
-define(ALGAE_REPRODUCTION_PROBABILITY, 0.2).
-define(ALGAE_INITIAL_ENERGY, 3).

-endif.