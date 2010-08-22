%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_client).

-export([start/2]).

-include_lib("../include/pongerl.hrl").
-include_lib("../dep/cecho/include/cecho.hrl").

start(Profile, OldFont) ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:start_color(),
    cecho:init_pair(?CLIENT_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    cecho:init_pair(?BALL_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    cecho:init_pair(?BG_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    cecho:init_pair(?FIELD_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_YELLOW),
    cecho:init_pair(?RESULT_PAIR, ?ceCOLOR_BLACK, ?ceCOLOR_GREEN),
    draw_field(),
    draw_result(),
    ID = rpc(connect_client, []),
    ClientStr = generate_spaces(?CX),
    BallStr = generate_spaces(?BX),
    spawn_link(fun() -> key_input_loop(ID, Profile, OldFont) end),
    spawn_link(fun() -> draw_game_loop(ID, ClientStr, BallStr) end).
    
key_input_loop(ID, Profile, OldFont) ->
    C = cecho:getch(),
    case C of
	$a ->
	    ok = rpc(change_client_position, [ID, ?UP]),
	    key_input_loop(ID, Profile, OldFont);
	$z ->
	    ok = rpc(change_client_position, [ID, ?DOWN]),
	    key_input_loop(ID, Profile, OldFont);
	$q ->
	    restore_terminal_font(Profile, OldFont),
	    cecho:curs_set(?ceCURS_NORMAL),
	    application:stop(cecho),
	    erlang:halt();
	_ ->
	    key_input_loop(ID, Profile, OldFont)
    end.

draw_game_loop(ID, ClientStr, BallStr) ->
    case rpc(get_state, [ID]) of
	not_started ->
	    timer:sleep(?ROUND_LENGTH);
	{restarting, Result} ->
	    draw_field(),
	    draw_result(Result);
	{C1, C2, Ball} ->
	    draw_game(ID, C1, C2, Ball, ClientStr, BallStr),
	    timer:sleep(?ROUND_LENGTH)
    end,
    draw_game_loop(ID, ClientStr, BallStr).

draw_game(ID, C1, C2, Ball, ClientStr, BallStr) ->
    Path1 = clean_path(ID, C1),
    Path2 = clean_path(ID, C2),
    PathB = clean_path(ID, Ball),
    draw(Path1, ClientStr, ?CY, ?CLIENT_PAIR, true),
    draw(Path2, ClientStr, ?CY, ?CLIENT_PAIR, true),
    draw(PathB, BallStr, ?BY, ?BALL_PAIR, true).

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
					      ?BG_PAIR, Pos),
    cecho:attron(?ceCOLOR_PAIR(Pair)),
    cecho:move(Y, X),
    cecho:addstr(Str),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(Pair)),
    draw([{X, Y + 1} | T], Str, HeightLeft - 1, Height, 
	 Pair, NewPrevious, RemPrev).

get_and_maybe_draw_previous({false, _Prev} = P, _Str, _Height, _Pair, _Pos) -> P;
get_and_maybe_draw_previous({true, Previous}, Str, Height, Pair, Pos) -> 
    draw(Previous, Str, Height, Pair, false),
    {false, Pos}.

draw_field() ->
    {H, W} = cecho:getmaxyx(),
    cecho:attron(?ceCOLOR_PAIR(?BG_PAIR)),
    draw_background(0, H, W),
    draw_limits(),
    cecho:refresh().

draw_limits() ->
    cecho:attron(?ceCOLOR_PAIR(?FIELD_PAIR)),
    cecho:move(?FY0 - 1, ?FX0 - 2),
    cecho:hline($ , ?FX + 4),
    cecho:move(?FY0, ?FX0 - 2),
    cecho:vline($ , ?FY),
    cecho:move(?FY0, ?FX0 - 1),
    cecho:vline($ , ?FY),
    cecho:move(?FY0, ?FX0 + ?FX),
    cecho:vline($ , ?FY),
    cecho:move(?FY0, ?FX0 + ?FX + 1),
    cecho:vline($ , ?FY),
    cecho:move(?FY0 + ?FY, ?FX0 - 2),
    cecho:hline($ , ?FX + 4),
    cecho:attroff(?ceCOLOR_PAIR(?FIELD_PAIR)).    

draw_background(Height, Height, _Width) -> ok;
draw_background(N, Height, Width) ->
    cecho:move(N, 0),
    cecho:hline($ , Width),
    draw_background(N + 1, Height, Width).

draw_result() ->
    draw_result({0, 0}).

draw_result({GC1, GC2}) ->
    pongerl_client_utils:draw_number(GC1, ?N1X0, ?N1Y0, 1),
    pongerl_client_utils:draw_number(GC2, ?N2X0, ?N2Y0, 2).

clean_path(ID, #client{} = C) ->
    clean_path(ID, C#client.path, []) ++ [{C#client.x, C#client.y}];
clean_path(ID, #ball{} = B) ->
    clean_path(ID, B#ball.path, []).
clean_path(ID, [{_X, _Y, ID} | T], Acc) ->
    clean_path(ID, T, Acc);
clean_path(ID, [{X, Y, _ID2} | T], Acc) ->
    clean_path(ID, T, [{X, Y} | Acc]);
clean_path(ID, [{X, Y} | T], Acc) ->
    clean_path(ID, T, [{X, Y} | Acc]);
clean_path(_ID, [], Acc) ->
    Acc.


generate_spaces(N) ->
    generate_spaces(N, "").
generate_spaces(0, Spaces) -> Spaces;
generate_spaces(N, Spaces) -> generate_spaces(N - 1, " " ++ Spaces).

restore_terminal_font(Profile, OldFont) ->
    ProfileStr = atom_to_list(Profile),
    Cmd = "gconftool --set /apps/gnome-terminal/profiles/" ++ ProfileStr ++ 
	"/font --type string \"" ++ OldFont ++ "\"",
    os:cmd(Cmd).
     
rpc(Function, Args) ->
    rpc:call(server@ardilla, pongerl_server, Function, Args).
