-module(syslog_drain_ws_handler).
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

init(_TransportName, Req, Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {<<"websocket">>, _} ->
      {upgrade, protocol, cowboy_websocket};
    _ ->
      {ok, Req, Opts}
  end.

%% Websocket
websocket_init(_TransportName, Req, Opts) ->
  {ok, Req, Opts}.

%% Will we have buffering? Or are we guaranteed to get the whole frame?
websocket_handle({text, Msg}, Req, Opts) ->
  syslog_pipeline:handle(Msg),
  {ok, Req, Opts};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

%% HTTP
handle(Req, Opts) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = maybe_handle(Method, HasBody, Req2),
  {ok, Req3, Opts}.

maybe_handle(<<"POST">>, true, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  syslog_pipeline:handle(Body),
  cowboy_req:reply(204, Req2);
maybe_handle(<<"POST">>, false, Req) ->
  %% They didn't send us anything
  cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_handle(_, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.
