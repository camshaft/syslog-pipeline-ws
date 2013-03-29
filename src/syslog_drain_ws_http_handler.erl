-module(syslog_drain_ws_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, Opts) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = maybe_handle(Method, HasBody, Req2, Opts),
  {ok, Req3, Opts}.

maybe_handle(<<"POST">>, true, Req, Opts) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  syslog_pipeline:handle(Body, Opts),
  cowboy_req:reply(204, Req);
maybe_handle(<<"POST">>, false, Req, _) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_handle(_, _, Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.
