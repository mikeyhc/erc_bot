-module(erc_bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%%%%%%%%%%%%%%%%%%%
%%% API Functions %%%
%%%%%%%%%%%%%%%%%%%%%

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% supervisor Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) -> {ok, { {one_for_one, 5, 10},
                   [ ?CHILD(conn_super, supervisor),
                     {erc_bot, {erc_bot, start_link, [conn_super]},
                      permanent, 5000, worker, [erc_bot]}
                   ]}}.
