-module(syslog_drain_ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%% API.

start(_StartType, _StartArgs) ->
  syslog_drain_ws_sup:start_link().

stop(_State) ->
  ok.
