%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_client).

-export([start/0]).

-include_lib("../include/pongerl.hrl").
-include_lib("../dep/cecho/include/cecho.hrl").

start() ->
    application:start(cecho),
    cecho:init_pair(1, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    cecho:init_pair(2, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    ID = pongerl_server:connect_client(),
    ClientStr = generate_spaces(?CX),
    BallStr = generate_spaces(?BX),
    spawn_link(key_input_loop(ID)),
    spawn_link(draw_game_loop(ClientStr, BallStr)).
    
key_input_loop(ID) ->
    C = cecho:getch(),
    case C of
	$a ->
	    ok = pongerl_server:change_client_position(ID, ?UP),
	    key_input_loop(ID);
	$z ->
	    ok = pongerl_server:change_client_position(ID, ?DOWN),
	    key_input_loop(ID);
	$q ->
	    application:stop(cecho),
	    erlang:halt()
    end.

draw_game_loop(ClientStr, BallStr) ->
    case pongerl_server:get_state() of
	not_started -> 
	    timer:sleep(?ROUND_LENGTH);
	{C1, C2, Ball} ->
	    draw_game(C1, C2, Ball, ClientStr, BallStr)
    end,
    draw_game_loop(ClientStr, BallStr).

draw_game(C1, C2, Ball, ClientStr, BallStr) ->
    draw_client(C1, ClientStr, ?CY),
    draw_client(C2, ClientStr, ?CY),
    draw_ball_path(lists:reverse(Ball#ball.path), BallStr, ?BY).

draw_client({_X, _Y}, _Str, 0) -> ok;
draw_client({X, Y}, Str, Height) ->
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    cecho:move(X, Y),
    cecho:addstr(Str),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    draw_client({X, Y + 1}, Str, Height - 1).

draw_ball_path([], _Str, 0)   -> ok;
draw_ball_path([_|T], Str, 0) -> draw_ball_path(T, Str, ?BY);
draw_ball_path([{X, Y}|_] = Path, Str, Height) ->
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(2)),
    cecho:move(X, Y),
    cecho:addstr(Str),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(2)),
    draw_client(Path, Str, Height - 1).
    

generate_spaces(N) ->
    generate_spaces(N, "").
generate_spaces(0, Spaces) -> Spaces;
generate_spaces(N, Spaces) -> generate_spaces(N - 1, " " ++ Spaces).
     
		      
