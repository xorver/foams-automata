%%%-------------------------------------------------------------------
%%% @author konrad
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2015 23:17
%%%-------------------------------------------------------------------
-module(worker).
-author("konrad").

-behaviour(gen_server).

-include("config.hrl").
-include("algae.hrl").
-include("foram.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    iteration = 0,
    map :: dict:dict(),
    left :: pid(),
    up :: pid(),
    right :: pid(),
    down :: pid(),
    master :: pid(),
    need_confirmation = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    random:seed(time()),
    Map = dict:new(),
    Map2 = foram:init(Map),
    Map3 = algae:init(Map2),
    {ok, #state{
        map = Map3
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({left, Left}, _From, State) ->
    {reply, ok, State#state{left = Left}};
handle_call({up, Up}, _From, State) ->
    {reply, ok, State#state{up = Up}};
handle_call({right, Right}, _From, State) ->
    {reply, ok, State#state{right = Right}};
handle_call({down, Down}, _From, State) ->
    {reply, ok, State#state{down = Down}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({step, ReplyTo},  State) ->
    NewState = step(ReplyTo, State),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(confirmed, #state{master = Master, need_confirmation = 1} = State) ->
    Master ! done,
    {noreply, State#state{need_confirmation = 0}};
handle_info(confirmed, #state{need_confirmation = NeedConfirmation} = State) ->
    {noreply, State#state{need_confirmation = NeedConfirmation - 1}};
handle_info({ReplyTo, #foram{}}, State) ->
    ReplyTo ! confirmed,
    {noreply, State#state{}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

step(ReplyTo, #state{map = Map, iteration = It} = State) ->
    map:dump(Map, It),
    Map2 = foram:move_all(Map),
    Map3 = foram:reproduce_all(Map2),
    Map4 = foram:starve_all(Map3),
    Map5 = foram:remove_dead(Map4),
    Map6 = algae:grow_all(Map5),
    Map7 = algae:reproduce_all(Map6),

    ReplyTo ! done,
    State#state{iteration = It + 1, map = Map7}.
