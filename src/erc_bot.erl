-module(erc_bot).

-behaviour(gen_server).

-include("erc_bot.hrl").

%% General API
-export([start_link/1]).

%% Connection API
-export([connect/2, connect/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(REAL_NAME, "erc_bot").

-record(state, {connections=#{} :: map(binary(), {pid(), connection()}),
                supervisor      :: pid()
               }).

%%%%%%%%%%%%%%%%%%%
%%% General API %%%
%%%%%%%%%%%%%%%%%%%

start_link(SuperPid) ->
    gen_server:start_link({local, erc_bot}, ?MODULE, SuperPid, []).

%%%%%%%%%%%%%%%%%%%%%%
%%% Connection API %%%
%%%%%%%%%%%%%%%%%%%%%%

connect(Server, Nick) -> connect(Server, 6667, Nick).
connect(Server, Port, Nick) ->
    gen_server:call(erc_bot, {connect, Server, Port, Nick}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% state Record Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_connection_state(R, N, S) ->
    Conns = R#state.connections,
    NewConns =maps:update(N, S, Conns),
    R#state{connections = NewConns}.


%%%%%%%%%%%%%%%%%%%%%
%%% IRC Functions %%%
%%%%%%%%%%%%%%%%%%%%%

do_connection(Super, Conn) ->
    {ok, Pid} = supervisor:start_child(Super, [Conn]),
    Pid.

create_connection(Server, Port, Nick, State) ->
    Conn = #connection{host = Server,
                       port = Port,
                       status = connecting,
                       nick = Nick},
    Pid = do_connection(State#state.supervisor, Conn),
    NewConns = maps:put(Server, {Pid, Conn}, State#state.connections),
    State#state{connections = NewConns}.

reuse_connection(C=#connection{status=disconnected}, State) ->
    NewState = set_connection_state(State, C, connecting),
    do_connection(State#state.supervisor, C),
    {ok, NewState};
reuse_connection(_S, State) -> {{error, already_connected}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(SuperPid) ->
    {ok, #state{supervisor=SuperPid}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({connect, Server, Port, Nick}, _From, State) ->
    case maps:find(Server, State#state.connections) of
        error ->
            NewState = create_connection(Server, Port, Nick, State),
            {reply, ok, NewState};
        {ok, Conn} ->
            {Ret, NewState} = reuse_connection(Conn, State),
            {reply, Ret, NewState}
    end;
handle_call(get_state, _From, State) -> {reply, {ok, State}, State}.

handle_cast(_Request, State) -> {no_reply, State}.
handle_info(_Request, State) -> {no_reply, State}.
