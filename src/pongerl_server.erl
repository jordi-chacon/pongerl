%%% @author Jordi Chacon <jordi.chacon@gmail.com>
%%% @copyright (C) 2010, Jordi Chacon

-module(pongerl_server).

-behaviour(gen_server).

%% API
-export([connect_client/0,
	 change_client_position/2,
	 get_state/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("../include/pongerl.hrl").

-define(SERVER, ?MODULE). 

-record(state, {clients = [], last_id = 0}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

connect_client() ->
    gen_server:call(?MODULE, connect_client).

get_state() ->
    gen_server:call(?MODULE, get_state).

change_client_position(ClientID, Direction) ->
    gen_server:call(?MODULE, {change_client_position, ClientID, Direction}).

handle_call(connect_client, _From, State) ->
    do_connect_client(State);
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

do_connect_client(State) ->
    ID = State#state.last_id,
    Clients = [Id|State#state.clients],
    case Clients of
	[ID1, ID2] -> ok = pongerl_engine:start_game(ID1, ID2),
	_          -> ok
    end,
    NewState = State#state{last_id = ID+ 1, clients = Clients},
    {reply, Id, NewState}.
    
do_get_state(State) ->
    Reply = pongerl_engine:get_state(),
    {reply, Reply, State}.

do_change_client_position(ClientID, Direction, State) ->
    ok = game_engine:change_client_position(ClientID, Direction),
    {reply, ok, State}.
