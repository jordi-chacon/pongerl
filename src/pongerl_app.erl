%% @author Jordi Chacon <jordi.chacon@gmail.com>
%% @copyright 2010 Jordi Chacon

%% @doc Callbacks for the pongerl application.

-module(pongerl_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("pongerl.hrl").

start(_, _) ->
    pongerl_sup:start_link().

stop(_) ->
    ok.
