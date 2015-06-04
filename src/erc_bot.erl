-module(erc_bot).

-include("irc_types.hrl").

-behaviour(gen_server).
-compile([export_all]).

% general api
-export([start_link/0, connect/3]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {sock,
                pid,
                nick}).

%%%%%%%%%%%%%%%%%%%
%%% general api %%%
%%%%%%%%%%%%%%%%%%%

start_link() -> gen_server:start_link({local, erc_bot}, ?MODULE, [], []).

connect(Host, Port, Nick) ->
    gen_server:call(erc_bot, {connect, Host, Port, Nick}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) -> {ok, #state{}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({connect, Host, Port, Nick}, _From, State) ->
    case State#state.sock of
        undefined ->
            {Ret, NewState} = do_connect(Host, Port),
            {reply, Ret, NewState#state{nick=Nick}};
        _         -> {reply, {error, already_connected}, State}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast({ircmsg, Msg}, State) -> {noreply, handle_message(Msg, State)};
handle_cast(disconnected, _State) -> {noreply, #state{}}.

handle_info(_Request, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% helper functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

do_connect(Host, Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [{active, false}]),
    Pid = spawn_link(fun() -> listen_loop(Sock) end),
    {ok, #state{sock=Sock, pid=Pid}}.

listen_loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Msg} ->
            MsgList = nl_split(Msg),
            lists:foreach(fun parse_and_send/1, MsgList),
            listen_loop(Sock);
        {error, closed} ->
            gen_server:cast(erc_bot, disconnected)
    end.

parse_and_send(Msg) ->
    Parsed = irc_parser:parse_message(Msg),
    gen_server:cast(erc_bot, {ircmsg, Parsed}).

handle_message(#irc_ping{response=R}, State) ->
    io:format("ping ~s~n", [R]),
    gen_tcp:send(State#state.sock, "PONG " ++ R ++ "\n"),
    State;
handle_message(#irc_notice{server=S, type=T, message=M}, State) ->
    io:format("notice ~s ~s: ~s~n", [S, T, M]),
    case lists:prefix("*** No ident", M) of
        true -> do_login(State);
        _    -> ok
    end,
    State;
handle_message(#irc_unknown{message=Msg}, State) ->
    io:format("unknown ~s~n",[Msg]),
    State.

nl_split(List) ->
    L = tl(lists:foldl(fun nl_split/2, [], List)),
    lists:map(fun lists:reverse/1, L).

nl_split(C, []) -> [[C]];
nl_split($\n, L) -> [[]|L];
nl_split(C, [H|T]) -> [[C|H]|T].

do_login(#state{sock=Sock, nick=Nick}) ->
    io:format("< NICK " ++ Nick ++ "~n"),
    gen_tcp:send(Sock, "NICK " ++ Nick ++ "\n"),
    io:format("< USER erc_bot 8 * : ERC Bot~n"),
    gen_tcp:send(Sock, "USER erc_bot 8 * : ERC Bot\n").
