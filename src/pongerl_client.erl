%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_client).

-export([start/0]).

-include_lib("../include/pongerl.hrl").
-include_lib("../dep/cecho/include/cecho.hrl").

start() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:start_color(),
    cecho:init_pair(?CLIENT_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    cecho:init_pair(?BALL_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    cecho:init_pair(?FIELD_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    draw_field(),
    ID = rpc(connect_client, []),
    ClientStr = generate_spaces(?CX),
    BallStr = generate_spaces(?BX),
    spawn_link(fun() -> key_input_loop(ID) end),
    spawn_link(fun() -> draw_game_loop(ID, ClientStr, BallStr) end).
    
key_input_loop(ID) ->
    C = cecho:getch(),
    case C of
	$a ->
	    ok = rpc(change_client_position, [ID, ?UP]),
	    key_input_loop(ID);
	$z ->
	    ok = rpc(change_client_position, [ID, ?DOWN]),
	    key_input_loop(ID);
	$q ->
	    cecho:curs_set(?ceCURS_NORMAL),
	    application:stop(cecho),
	    erlang:halt();
	_ ->
	    key_input_loop(ID)
    end.

draw_game_loop(ID, ClientStr, BallStr) ->
    case rpc(get_state, [ID]) of
	not_started -> 
	    timer:sleep(?ROUND_LENGTH);
	{C1, C2, Ball} ->
	    draw_game(ID, C1, C2, Ball, ClientStr, BallStr),
	    timer:sleep(?ROUND_LENGTH)
    end,
    draw_game_loop(ID, ClientStr, BallStr).

draw_game(ID, C1, C2, Ball, ClientStr, BallStr) ->
    Path1 = clean_client_path(ID, C1#client.path),
    Path2 = clean_client_path(ID, C2#client.path),
    draw(Path1, ClientStr, ?CY, ?CLIENT_PAIR, true),
    draw(Path2, ClientStr, ?CY, ?CLIENT_PAIR, true),
    draw(lists:reverse(Ball#ball.path), BallStr, ?BY, ?BALL_PAIR, true).

draw([], _Str, _Height, _Pair, _RemPrev) -> 
    ok;
draw(Path, Str, Height, Pair, RemPrev) -> 
    draw(Path, Str, Height, Height, Pair, {false, hd(Path)}, RemPrev).
draw([], _Str, _HeightLeft, _Height, _Pair, _Previous, _RemPrev) -> 
    ok;
draw([_|T], Str, 0, Height, Pair, {_, P}, RemPrev) -> 
    draw(T, Str, Height, Height, Pair, {RemPrev, [P]}, RemPrev);
draw([{X, Y} = Pos|T], Str, HeightLeft, Height, Pair, Previous, RemPrev) ->
    NewPrevious = get_and_maybe_draw_previous(Previous, Str, Height, 
					      ?FIELD_PAIR, Pos),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(Pair)),
    cecho:move(Y, X),
    cecho:addstr(Str),
    cecho:refresh(),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(Pair)),
    draw([{X, Y + 1} | T], Str, HeightLeft - 1, Height, 
	 Pair, NewPrevious, RemPrev).

get_and_maybe_draw_previous({false, _Prev} = P, _Str, _Height, _Pair, _Pos) -> P;
get_and_maybe_draw_previous({true, Previous}, Str, Height, Pair, Pos) -> 
    draw(Previous, Str, Height, Pair, false),
    {false, Pos}.

draw_field() ->
    Str = generate_spaces(?XF - ?X0),
    draw([{?X0, ?Y0}], Str, ?YF, ?FIELD_PAIR, false).

clean_client_path(ID, Path) ->
    clean_client_path(ID, Path, []).
clean_client_path(ID, [{_X, _Y, ID} | T], Acc) ->
    clean_client_path(ID, T, Acc);
clean_client_path(ID, [{X, Y, _ID2} | T], Acc) ->
    clean_client_path(ID, T, [{X, Y} | Acc]);
clean_client_path(ID, [{X, Y} | T], Acc) ->
    clean_client_path(ID, T, [{X, Y} | Acc]);
clean_client_path(_ID, [], Acc) ->
    Acc.


generate_spaces(N) ->
    generate_spaces(N, "").
generate_spaces(0, Spaces) -> Spaces;
generate_spaces(N, Spaces) -> generate_spaces(N - 1, " " ++ Spaces).
     
rpc(Function, Args) ->
    rpc:call(server@ardilla, pongerl_server, Function, Args).
