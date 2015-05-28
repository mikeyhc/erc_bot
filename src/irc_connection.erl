-module(irc_connection).

-include("erc_bot.hrl").

-export([start_link/1]).
-export([loop/2, init/1]).

start_link(Connection) ->
    Pid = spawn_link(?MODULE, init, [Connection]),
    {ok, Pid}.

init(Conn) ->
    {ok, Sock} = gen_tcp:connect(Conn#connection.host,
                                 Conn#connection.port,
                                 [{active, false}]),
    loop(Sock, Conn).

loop(Sock, Conn) ->
    {ok, Msg} = gen_tcp:recv(Sock, 0),
    ok = io:format("~s~n", [Msg]),
    irc_connection:loop(Sock, Conn).
