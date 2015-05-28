-module(irc_connection).

-include("erc_bot.hrl").

-export([start_link/1]).
-export([loop/1, init/1]).

-record(state, {sock,
                conn,
                listeners}).

start_link(Connection) ->
    Pid = spawn_link(?MODULE, init, [Connection]),
    {ok, Pid}.

do_connect(Conn) ->
    {ok, Sock} = gen_tcp:connect(Conn#connection.host,
                                 Conn#connection.port,
                                 [{active, false}]),
    Sock.

init(Conn=#connection{}) ->
    Sock = do_connect(Conn),
    loop(#state{sock=Sock, conn=Conn, listeners=[]});
init(State=#state{}) ->
    Sock = do_connect(State#state.conn),
    loop(State#state{sock=Sock}).

loop(State) ->
    try
        {ok, Msg} = gen_tcp:recv(State#state.sock, 0),
        ok = io:format("~s~n", [Msg]),
        NewState = receive
            {add_listener, Pid} ->
                NewListen = [Pid|State#state.listeners],
                State#state{listeners=NewListen};
            {get_state, Pid} ->
                Pid ! State,
                State
        after 500 -> State end,
        irc_connection:loop(NewState)
    catch
        _ -> irc_connection:init(State);
        error:_ -> irc_connection:init(State)
    end.
