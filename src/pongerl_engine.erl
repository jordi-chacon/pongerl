%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_engine).

-behaviour(gen_server).

%% API
-export([start_game/2,
	 get_state/0,
	 change_client_position/2]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("../include/pongerl.hrl").

-define(SERVER, ?MODULE). 

-record(state, {clients = [], ball = #ball{}}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

start_game(ID1, ID2) ->
    gen_server:call(?MODULE, {start_game, ID1, ID2}).

get_state() ->
    gen_server:call(?MODULE, get_state).

change_client_position(ClientID, Direction) ->
    gen_srever:call(?MODULE, {change_client_position, ClientID, Direction}).

handle_call({start_game, ID1, ID2}, _From, State) ->
    do_start_game(ID1, ID2, State);
handle_call(get_state, _From, State) ->
    do_get_state(State);
handle_call({change_client_position, ClientID, Direction}, _From, State) ->
    do_change_client_position(ClientID, Direction, State);
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

do_start_game(ID1, ID2, _State) ->
    C1 = get_initial_position_client(first),
    C2 = get_initial_position_client(second),
    Ball = get_initial_position_ball(),
    NewState = #state{clients = [{ID1, C1}, {ID2, C2}], ball = Ball},
    {reply, ok, NewState}.

do_get_state(State) ->
    CPos = State#state.clients,
    BallPos = get_ball_position(State#state.ball),
    {reply, {CPos, BallPos}, State}.

do_change_client_position(ClientID, Direction, State) ->
    CPos = get_client_new_position(ClientID, State#state.clients, Direction),
    PL1 = proplists:delete(ClientID, State#state.clients),
    NewState = State#state{clients = [{ClientID, CPos} | PL1]},
    {reply, ok, NewState}.

get_initial_position_client(first)  -> {?X0 + 2, ?Y0 div 2};
get_initial_position_client(second) -> {?XF - 2, ?Y0 div 2}.

get_initial_position_ball() -> #ball{}.

get_ball_position(#ball{x = X, y = Y}) -> {X, Y}.

get_client_new_position(ID, [{ID, {X, Y}} | _T], ?UP)   -> {X - 1, Y};
get_client_new_position(ID, [{ID, {X, Y}} | _T], ?DOWN) -> {X + 1, Y};
get_client_new_position(ID, [{_ID, _Pos} | T], Dir) -> 
    get_client_new_position(ID, T, Dir).
    
