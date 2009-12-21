-module(zfor_app).
-behaviour(application).
-include("zfor_common.hrl").
-export([start/2, stop/1]).
-compile([debug_info, bin_opt_info]).

%% ====================== Exported API =======================

% @doc Start ZFOR application.
% @spec start(
%			StartType::('normal' | {'takeover', Node::node()} | {'failover', Node::node()}),
%			StartArgs::term()
%		) -> {'ok', pid()} | {'ok', pid(), term()} | {'error', term()}
% @end
% {{{
-spec start(
	StartType::('normal' | {'takeover', Node::node()} | {'failover', Node::node()}),
	StartArgs::term()
) -> {'ok', pid()} | {'ok', pid(), term()} | {'error', term()}.

start(_Type, _Args) ->
	% 从应用配置参数环境中读取conf_path参数
	case application:get_env(zfor, 'conf_path') of
		{ok, Val} ->
			ConfPath = Val;
		_ ->
			ConfPath = ?DEFAULT_CONFIG_PATH
	end,
	zfor_sup:start_link(ConfPath).
% }}}

% @doc Callback when ZFOR application is stopped.
% @spec stop(State::term()) -> 'ok'
% @end
% {{{
-spec stop(State::term()) -> 'ok'.

stop(_State) ->
	ok.
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

