-module(erc_bot).

-behaviour(gen_server).

%% General API
-export([start_link/0]).

%% Connection API
-export([connect/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(REAL_NAME, "erc_bot").

-record(connection, {server_name :: binary(),
                     status      :: connected | connecting | disconnected,
                     nick        :: binary()}).
-type connection() :: #connection{}.

-record(state, {connections=#{} :: map(binary(), connection())
               }).

%%%%%%%%%%%%%%%%%%%
%%% General API %%%
%%%%%%%%%%%%%%%%%%%

start_link() -> gen_server:start_link({local, erc_bot}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%
%%% Connection API %%%
%%%%%%%%%%%%%%%%%%%%%%

connect(Server, Nick) -> gen_server:call(erc_bot, {connect, Server, Nick}).

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

do_connection(_Server, _Nick) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) -> {ok, #state{}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% server already exists
handle_call({connect, Server, Nick}, _From, State) ->
    Connections = State#state.connections,
    case maps:find(Server, Connections) of
        error ->
            Conn = #connection{server_name = Server,
                               status = connecting,
                               nick = Nick},
            NewConns = maps:put(Server, Conn, Connections),
            NewState = State#state{connections = NewConns},
            Status = do_connection(Server, Nick),
            {reply, Status, NewState};
        {ok, S} ->
            case S#connection.status of
                disconnected ->
                    NewState = set_connection_state(State, Server, connecting),
                    Status = do_connection(Server, Nick),
                    {reply, Status, NewState};
                _ -> {reply, {error, already_connected}, State}
            end
    end.



handle_cast(_Request, State) -> {no_reply, State}.
handle_info(_Request, State) -> {no_reply, State}.
