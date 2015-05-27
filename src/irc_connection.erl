-module(irc_connection).

-export([start_link/0]).

-export([loop/0]).

start_link() ->
    Pid = spawn_link(?MODULE, loop, []),
    {ok, Pid}.

loop() ->
    receive
        _ -> io:format("looping!~n", [])
    end,
    erlang:hibernate(?MODULE, loop, []).
