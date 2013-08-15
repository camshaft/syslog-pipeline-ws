%% @private
-module(syslog_pipeline_ws_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

start_link() ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.
