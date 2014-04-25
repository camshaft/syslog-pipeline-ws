-module(syslog_pipeline_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
%% Websocket
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
%% HTTP
-export([handle/2]).
-export([terminate/3]).

init(_TransportName, Req, Pipeline) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {<<"websocket">>, _} ->
      {upgrade, protocol, cowboy_websocket};
    _ ->
      {ok, Req, Pipeline}
  end.

%% Websocket
websocket_init(_TransportName, Req, Pipeline) ->
  {Host, Req2} = cowboy_req:binding(host, Req),
  {ok, Req2, {Pipeline, Host}}.

websocket_handle({text, Msg}, Req, {Pipeline, Host}) ->
  pipeline(Pipeline, Host, Msg),
  {ok, Req, Pipeline};
websocket_handle(_Data, Req, Pipeline) ->
  {ok, Req, Pipeline}.

websocket_info(_Info, Req, Pipeline) ->
  {ok, Req, Pipeline}.

websocket_terminate(_Reason, _Req, _Pipeline) ->
  ok.

%% HTTP
handle(Req, Pipeline) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Log, Host, Req3} = maybe_handle(Method, cowboy_req:has_body(Req2), Req2),
  pipeline(Pipeline, Host, Log),
  cowboy_req:reply(200, Req2),
  {ok, Req3, Pipeline}.

maybe_handle(<<"GET">>, _, Req) ->
  {Log, Req2} = cowboy_req:qs_val(<<"msg">>, Req, <<>>),
  {Host, Req3} = cowboy_req:binding(host, Req2),
  {ok, Log, Host, Req3};
maybe_handle(<<"POST">>, true, Req) ->
  {ok, Log, Req2} = cowboy_req:body(Req),
  {Host, Req3} = cowboy_req:binding(host, Req2),
  {ok, Log, Host, Req3};
maybe_handle(<<"POST">>, false, Req) ->
  {{error, missing_body}, Req};
maybe_handle(_, _, Req) ->
  {{error, method_not_allowed}, Req}.

pipeline(Pipeline, undefined, Log) ->
  syslog_pipeline:handle(Pipeline, Log);
pipeline(Pipeline, Host, Log) ->
  syslog_pipeline:handle(Pipeline, Log, [{hostname, Host}]).

terminate(_Reason, _Req, _Pipeline) ->
  ok.
