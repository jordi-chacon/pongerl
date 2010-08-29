%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_client_utils).

-export([draw_number/4]).

-include_lib("../include/pongerl.hrl").
-include_lib("../dep/cecho/include/cecho.hrl").

draw_number(0, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(1, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 4, "  ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(2, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(3, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 2, "    ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(4, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 0, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 4, "  ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(5, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(6, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(7, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 4, "  ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(8, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(9, X, Y, _) ->
    cecho:attron(?ceCOLOR_PAIR(?RESULT_PAIR)),
    adapt_to_factor(Y, 0, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 0, "  ", ?NFACTOR),
    adapt_to_factor(Y, 1, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 2, X, 0, "      ", ?NFACTOR),
    adapt_to_factor(Y, 3, X, 4, "  ", ?NFACTOR),
    adapt_to_factor(Y, 4, X, 0, "      ", ?NFACTOR),
    cecho:refresh(),
    cecho:attroff(?ceCOLOR_PAIR(?RESULT_PAIR));
draw_number(N, X, Y, 1) ->
    FirstDigit = N div 10,
    SecondDigit = N rem 10,
    draw_number(FirstDigit, X - (?NX - 1) * ?NFACTOR , Y, 1),
    draw_number(SecondDigit, X, Y, 1);
draw_number(N, X, Y, 2) ->
    FirstDigit = N div 10,
    SecondDigit = N rem 10,
    draw_number(FirstDigit, X, Y, 1),
    draw_number(SecondDigit, X + (?NX + 1) * ?NFACTOR, Y, 1).
    
adapt_to_factor(Y, OffsetY, X, OffsetX, Str, Factor) ->
    adapt_to_factor2(Y + OffsetY * Factor, X + OffsetX * Factor, 
		     factor_spaces(Str, Factor), Factor).

adapt_to_factor2(_Y, _X, _Str, 0) -> ok;
adapt_to_factor2(Y, X, Str, Left) ->
    cecho:move(Y, X),
    cecho:addstr(Str),
    adapt_to_factor2(Y + 1, X, Str, Left - 1).

factor_spaces(Str, N) ->
    factor_spaces(Str, N, []).
factor_spaces(_Str, 0, Acc) ->
    Acc;
factor_spaces(Str, N, Acc) ->
    factor_spaces(Str, N - 1, Str ++ Acc).
    
