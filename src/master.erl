%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(master).
-author("Tomasz Lichon").

-behaviour(gen_server).

-include("config.hrl").

%% API
-export([start_link/0, run/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(run, _From, State) ->
    master:run(),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run() ->
    WorkersTabId = ets:new(workers, []),

    utils:for(1, ?WORKERS_WIDTH, fun(Row) ->
        utils:for(1, ?WORKERS_WIDTH, fun(Column) ->
            io:format("Starting ~B ~B ~n", [Row, Column]),
            {ok, Pid} = worker:start_link(),
            ets:insert(WorkersTabId, {{Row, Column}, Pid})
        end)
    end),

    ets:foldl(
        fun({{Row, Column}, This}, Acc) ->
            Left = find(WorkersTabId, Row, Column - 1),
            Up = find(WorkersTabId, Row - 1, Column),
            Right = find(WorkersTabId, Row, Column + 1),
            Down = find(WorkersTabId, Row + 1, Column),

            ok = gen_server:call(This, {left, Left}),
            ok = gen_server:call(This, {up, Up}),
            ok = gen_server:call(This, {right, Right}),
            ok = gen_server:call(This, {down, Down}),
            Acc
        end,
        {},
        WorkersTabId),

    utils:for(1, ?STEPS, fun(StepNum) ->
        WorkersNum =
            ets:foldl(
                fun({_, Pid}, Acc) ->
                    ok = gen_server:cast(Pid, {step, self()}),
                    Acc + 1
                end,
                0,
                WorkersTabId),

        utils:for(1, WorkersNum, fun(_) -> receive done -> ok end end),
        io:format("Step ~B done~n", [StepNum])
    end).

find(TabId, Row, Column) ->
    case ets:lookup(TabId, {Row, Column}) of
        [] -> undefined;
        [{_, Value}] -> Value
    end.

