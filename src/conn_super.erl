-module(conn_super).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% supervisor Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) -> {ok, { {simple_one_for_one, 3, 60},
                   [{connection,
                     {irc_connection, start_link, []},
                     temporary, 1000, worker, [musicians]}
                   ]}}.
