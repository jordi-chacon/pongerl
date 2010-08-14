%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_engine).

-behaviour(gen_server).

%% API
-export([start_game/2,
	 restart_game/0,
	 get_state/0,
	 change_client_position/2,
	 run_engine/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("../include/pongerl.hrl").

-define(SERVER, ?MODULE). 

-record(state, {clients = [], ball = #ball{}, path = []}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

start_game(ID1, ID2) ->
    gen_server:call(?MODULE, {start_game, ID1, ID2}).

restart_game() ->
    gen_server:cast(?MODULE, restart_game).

get_state() ->
    gen_server:call(?MODULE, get_state).

change_client_position(ClientID, Direction) ->
    gen_server:call(?MODULE, {change_client_position, ClientID, Direction}).

run_engine() ->
    gen_server:call(?MODULE, run_engine).

handle_call({start_game, ID1, ID2}, _From, State) ->
    do_start_game(ID1, ID2, State);
handle_call(get_state, _From, State) ->
    do_get_state(State);
handle_call({change_client_position, ClientID, Direction}, _From, State) ->
    do_change_client_position(ClientID, Direction, State);
handle_call(run_engine, _From, State) ->
    do_run_engine(State);
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(restart_game, State) ->
    do_restart_game(State);
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
    register(run_engine_caller, run_engine_caller()),
    {reply, ok, NewState}.

do_restart_game(State) ->
    Ball = get_initial_position_ball(),
    NewState = State#state{ball = Ball},
    register(run_engine_caller, run_engine_caller()),
    {noreply, NewState}.

do_get_state(State) ->
    CPos = State#state.clients,
    BallPos = get_ball_position(State#state.ball),
    {reply, {CPos, BallPos}, State}.

do_change_client_position(ClientID, Direction, State) ->
    CPos = get_client_new_position(ClientID, State#state.clients, Direction),
    PL1 = proplists:delete(ClientID, State#state.clients),
    NewState = State#state{clients = [{ClientID, CPos} | PL1]},
    {reply, ok, NewState}.

do_run_engine(#state{clients = [{_ID1, P1}, {_ID2, P2}], ball = Ball} =State) ->
    case run_steps(P1, P2, Ball) of
	{Path, NewBall} -> ok;
	{Path, NewBall, end_of_game} -> run_engine_caller ! die
    end,
    {reply, ok, State#state{ball = NewBall, path = Path}}.
    

%%%===================================================================
%%% More internal functions
%%%===================================================================

get_initial_position_client(first)  -> {?X0 + 2, ?Y0 div 2};
get_initial_position_client(second) -> {?XF - 2, ?Y0 div 2}.

get_initial_position_ball() -> #ball{}.

get_ball_position(#ball{x = X, y = Y}) -> {X, Y}.

get_client_new_position(ID, [{ID, {X, Y}} | _T], ?UP)   -> {X, Y - 1};
get_client_new_position(ID, [{ID, {X, Y}} | _T], ?DOWN) -> {X, Y + 1};
get_client_new_position(ID, [{_ID, _Pos} | T], Dir) -> 
    get_client_new_position(ID, T, Dir).

run_steps(P1, P2, Ball) ->
    run_step(P1, P2, Ball, [], Ball#ball.speed).

% steps done
run_step(_P1, _P2, Ball, Path, 0) ->
    {Path, Ball};
% end of game
run_step({X1, _Y1}, {X2, _Y2}, #ball{x = BX} = Ball, Path, _Steps) 
  when X1 =:= BX orelse X2 =:= BX ->
    {Path, Ball, end_of_game};    
run_step(P1 = {X1, Y1}, P2, #ball{x = XB, y = YB} = Ball, Path, Steps) 
  when XB =:= X1 + 1->
    Degrees = Ball#ball.degrees,
    case YB >= Y1 andalso YB =< Y1 + ?CY of
	true ->
	    % client 1 touches the ball
	    NewDegrees = get_new_degree(Degrees, opposite),
	    NewPath = [{XB, YB} | Path],
	    NewBall = Ball#ball{degrees = NewDegrees};
	false ->
	    % client 1 receives a goal
	    {NX, NY} = get_next_ball_position(XB, YB, Degrees),
	    NewPath = [{NX, NY} | Path],
	    NewBall = Ball#ball{x = NX, y = NY}
    end,
    run_step(P1, P2, NewBall, NewPath, Steps - 1);
run_step(P1, P2 = {X2, Y2}, #ball{x = XB, y = YB} = Ball, Path, Steps) 
  when XB =:= X2 - 1 ->
    Degrees = Ball#ball.degrees,
    case YB >= Y2 andalso YB =< Y2 + ?CY of
	true ->
	    % client 2 touches the ball
	    NewDegrees = get_new_degree(Degrees, opposite),
	    NewPath = [{XB, YB} | Path],
	    NewBall = Ball#ball{degrees = NewDegrees};
	false ->
	    % client 2 receives a goal
	    {NX, NY} = get_next_ball_position(XB, YB, Degrees),
	    NewPath = [{NX, NY} | Path],
	    NewBall = Ball#ball{x = NX, y = NY}
    end,
    run_step(P1, P2, NewBall, NewPath, Steps - 1);
run_step(P1, P2, #ball{x = X, y = Y} = Ball, Path, Steps) ->
    Degrees = Ball#ball.degrees,
    {NX, NY} = get_next_ball_position(X, Y, Degrees),
    NewPath = [{NX, NY} | Path],
    run_step(P1, P2, Ball#ball{x = NX, y = NY}, NewPath, Steps - 1).
    
get_new_degree(180, opposite) -> 0;
get_new_degree(0, opposite)   -> 180.

get_next_ball_position(X, Y, 180) -> {X - 1, Y};
get_next_ball_position(X, Y, 0)   -> {X + 1, Y}.
    
    
% loop to run the game engine every xx ms
run_engine_caller() ->
    receive
	die ->
	    timer:sleep(2000),
	    restart_game()
    after 100 ->
	    ok = run_engine(),
	    run_engine_caller()
    end.
    
