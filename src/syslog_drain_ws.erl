%%
%% syslog_drain_ws.erl
%% syslog_drain_ws entry point
%%
-module (syslog_drain_ws).

-export([start_server/4]).
-export([stop_listener/1]).
-export([set_env/3]).
-export([get_value/3]).

%% @doc Start a syslog drain listener.
-spec start_server(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, ProtoOpts)
    when is_integer(NbAcceptors), NbAcceptors > 0 ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", syslog_drain_ws_http_handler, []},
      {"/ws", syslog_drain_ws_handler, []}
    ]}
  ]),
  cowboy:start_http(Ref, NbAcceptors, TransOpts,
    [{env, [{dispatch, Dispatch}|ProtoOpts]}]).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
  ranch:stop_listener(Ref).

%% @doc Convenience function for setting an environment value.
%%
%% Allows you to update live an environment value; mainly used to
%% add/remove parsers and emitters
-spec set_env(any(), atom(), any()) -> ok.
set_env(Ref, Name, Value) ->
  cowboy:set_env(Ref, Name, Value).
