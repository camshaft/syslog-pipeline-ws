%%
%% syslog_pipeline_ws.erl
%% syslog_pipeline_ws entry point
%%
-module (syslog_pipeline_ws).

-export([start_server/5]).
-export([stop_listener/1]).
-export([set_env/3]).

%% @doc Start a syslog pipeline listener.
-spec start_server(any(), non_neg_integer(), any(), any(), atom()) -> {ok, pid()}.
start_server(Ref, NbAcceptors, TransOpts, ProtoOpts, Pipeline)
    when is_integer(NbAcceptors), NbAcceptors > 0 ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", syslog_pipeline_ws_handler, Pipeline},
      {"/:host", syslog_pipeline_ws_handler, Pipeline}
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
