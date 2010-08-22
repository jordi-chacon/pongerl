%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_engine).

-behaviour(gen_server).

%% API
-export([start_game/2,
	 restart_game/0,
	 get_state/1,
	 change_client_position/2,
	 run_engine/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("../include/pongerl.hrl").

-define(SERVER, ?MODULE). 

-record(state, {client1 = empty, client2 = empty, result = {0, 0},
		ball = #ball{}, status = not_started}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

start_game(ID1, ID2) ->
    gen_server:call(?MODULE, {start_game, ID1, ID2}).

restart_game() ->
    gen_server:cast(?MODULE, restart_game).

get_state(ID) ->
    gen_server:call(?MODULE, {get_state, ID}).

change_client_position(ClientID, Direction) ->
    gen_server:call(?MODULE, {change_client_position, ClientID, Direction}).

run_engine() ->
    gen_server:call(?MODULE, run_engine).

handle_call({start_game, ID1, ID2}, _From, State) ->
    do_start_game(ID1, ID2, State);
handle_call({get_state, ID}, _From, State) ->
    do_get_state(ID, State);
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
    C1 = get_initial_position_client(ID1, first),
    C2 = get_initial_position_client(ID2, second),
    Ball = get_initial_position_ball(),
    NewState = #state{client1 = C1, client2 = C2, ball = Ball, status = started},
    spawn(fun() -> run_engine_caller() end),
    {reply, ok, NewState}.

do_restart_game(State = #state{client1 = C1, client2 = C2}) ->
    NC1 = get_initial_position_client(C1#client.id, first),
    NC2 = get_initial_position_client(C2#client.id, second),
    Ball = get_initial_position_ball(),
    NewState = State#state{client1 = NC1, client2 = NC2, 
			   ball = Ball, status = restarting},
    spawn(fun() -> run_engine_caller() end),
    {noreply, NewState}.

do_get_state(ID, State) ->
    C1 = State#state.client1,
    C2 = State#state.client2,
    {Reply, NewState} = 
	case State#state.status of
	    started -> 
		NC1 = flag_and_clean_path(ID, C1),
		NC2 = flag_and_clean_path(ID, C2),
		NB = flag_and_clean_path(ID, State#state.ball),
		NS = State#state{client1 = NC1, client2 = NC2, ball = NB},
		{{C1, C2, State#state.ball}, NS};
	    not_started -> {not_started, State};
	    restarting  -> 
		{{restarting, State#state.result}, 
		 State#state{status = {restarting, ID}}};
	    {restarting, ID} = St -> 
		{{restarting, State#state.result}, State#state{status = St}};
	    {restarting, _ID2} -> 
		{{restarting, State#state.result}, 
		 State#state{status = not_started}}
	end,
    {reply, Reply, NewState}.

do_change_client_position(ClientID, Direction, 
			  State = #state{client1 = C1, client2 = C2}) ->
    ID1 = C1#client.id,
    ID2 = C2#client.id,
    NewState = case ClientID of
		   ID1 ->
		       NC = get_client_new_position(C1, Direction),
		       State#state{client1 = NC};
		   ID2 ->
		       NC = get_client_new_position(C2, Direction),
		       State#state{client2 = NC}
	       end,
    {reply, ok, NewState}.

do_run_engine(#state{client1 = C1, client2 = C2, ball = Ball} = State) ->
    #client{x = X1, y = Y1} = C1,
    #client{x = X2, y = Y2} = C2,
    {Reply, State2} = 
	case run_steps({X1, Y1}, {X2, Y2}, Ball) of
	    {NewBall, end_of_game, client1} -> 
		{GoalsC1, GoalsC2} = State#state.result,
		{end_of_game, State#state{result = {GoalsC1 + 1, GoalsC2}}};
	    {NewBall, end_of_game, client2} -> 
		{GoalsC1, GoalsC2} = State#state.result,
		{end_of_game, State#state{result = {GoalsC1, GoalsC2 + 1}}};
	    NewBall -> 
		{ok, State}
    end,
    {reply, Reply, State2#state{ball = NewBall, status = started}}.
    

%%%===================================================================
%%% More internal functions
%%%===================================================================

get_initial_position_client(ID, first) ->
    X = ?FX0 + 2,
    Y = (?FY0 + ?FY) div 2 - ?CY div 2,
    #client{id = ID, x = X, y = Y, path = [{X, Y}]};
get_initial_position_client(ID, second) ->
    X = ?FX0 + ?FX - 1 - ?CX,
    Y = (?FY0 + ?FY) div 2 - ?CY div 2,
    #client{id = ID, x = X, y = Y, path = [{X, Y}]}.

get_initial_position_ball() -> #ball{}.

get_client_new_position(#client{y = Y, x = X} = C, ?UP) when Y > ?FY0 ->
    C#client{y = Y - 1, path = [{X, Y} | C#client.path]};
get_client_new_position(#client{y = Y, x = X} = C, ?DOWN) 
  when Y + ?CY < ?FY0 + ?FY ->
    C#client{y = Y + 1, path = [{X, Y} | C#client.path]};
get_client_new_position(C, _Direction) -> C.

flag_and_clean_path(ID, #client{} = C) ->
    NewPath = flag_and_clean_path(ID, C#client.path, []),
    C#client{path = NewPath};
flag_and_clean_path(ID, #ball{} = B) ->
    NewPath = flag_and_clean_path(ID, B#ball.path, []),
    B#ball{path = NewPath}.
flag_and_clean_path(_ID, [], Acc) ->
    lists:reverse(Acc);
flag_and_clean_path(ID, [{X, Y} | T], Acc) ->
    flag_and_clean_path(ID, T, [{X, Y, ID} | Acc]);
flag_and_clean_path(ID, [{X, Y, ID} | T], Acc) ->
    flag_and_clean_path(ID, T, [{X, Y, ID} | Acc]);
flag_and_clean_path(ID, [{_X, _Y, _ID2} | T], Acc) ->
    flag_and_clean_path(ID, T, Acc).

run_steps(P1, P2, #ball{x = X, y = Y, speed = Speed, path = Path} = Ball) ->
    run_step(P1, P2, Ball, [{X, Y} | Path], Speed).

% steps done
run_step(_P1, _P2, Ball, Path, 0) ->
    Ball#ball{path = Path};
% end of game
run_step({X1, _Y1}, {_X2, _Y2}, #ball{x = XB} = Ball, Path, _Steps) 
  when X1 =:= XB ->
    {Ball#ball{path = Path}, end_of_game, client2};
run_step({_X1, _Y1}, {X2, _Y2}, #ball{x = XB} = Ball, Path, _Steps) 
  when XB + ?BX - 1 =:= X2 ->
    {Ball#ball{path = Path}, end_of_game, client1};
run_step(P1 = {X1, Y1}, P2, #ball{x = XB, y = YB} = Ball, Path, Steps) 
  when XB =:= X1 + 1 ->
    Degrees = Ball#ball.degrees,
    case YB >= Y1 andalso YB =< Y1 + ?CY - 1 of
	true when Degrees > 90 andalso Degrees < 270 ->
	    % client 1 touches the ball
	    NewDegrees = get_new_degree(Degrees, opposite),
	    NewPath = [{XB, YB} | Path],
	    NewBall = Ball#ball{degrees = NewDegrees};
	true ->
	    % client 1 has changed the direction of the ball
	    {NX, NY} = get_next_ball_position(XB, YB, Degrees),
	    NewPath = [{NX, NY} | Path],
	    NewBall = Ball#ball{x = NX, y = NY};
	false ->
	    % client 1 receives a goal
	    {NX, NY} = get_next_ball_position(XB, YB, Degrees),
	    NewPath = [{NX, NY} | Path],
	    NewBall = Ball#ball{x = NX, y = NY}
    end,
    run_step(P1, P2, NewBall, NewPath, Steps - 1);
run_step(P1, P2 = {X2, Y2}, #ball{x = XB, y = YB} = Ball, Path, Steps) 
  when XB + ?BX =:= X2 ->
    Degrees = Ball#ball.degrees,
    case YB >= Y2 andalso YB =< Y2 + ?CY - 1 of
	true when Degrees < 90 orelse Degrees > 270 ->
	    % client 2 touches the ball
	    NewDegrees = get_new_degree(Degrees, opposite),
	    NewPath = [{XB, YB} | Path],
	    NewBall = Ball#ball{degrees = NewDegrees};
	true ->
	    % client 2 has changed the direction of the ball
	    {NX, NY} = get_next_ball_position(XB, YB, Degrees),
	    NewPath = [{NX, NY} | Path],
	    NewBall = Ball#ball{x = NX, y = NY};
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
    timer:sleep(?ROUND_LENGTH),
    case run_engine() of
	ok          -> run_engine_caller();
	end_of_game -> timer:sleep(?PAUSE_AFTER_GOAL), restart_game()
    end.
    
