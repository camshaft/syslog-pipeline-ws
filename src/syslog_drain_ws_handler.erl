-module(syslog_drain_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
  {ok, Req, Opts}.

%% Will we have buffering? Or are we guaranteed to get the whole frame?
websocket_handle({text, Msg}, Req, Opts) ->
  syslog_pipeline:handle(Msg, Opts),
  {ok, Req, Opts};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
