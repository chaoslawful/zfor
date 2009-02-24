% ZFOR应用，作为整个ZFOR服务树的启动控制入口
-module(zfor_app).
-behaviour(application).
-include("zfor_common.hrl").
-export([start/2,stop/1]).
%-compile([debug_info,export_all]).

start(_Type,_Args)->
	% 从应用配置参数环境中读取conf_path参数
	case application:get_env(zfor, 'conf_path') of
		{ok, Val} ->
			ConfPath=Val;
		_ ->
			ConfPath=?DEFAULT_CONFIG_PATH
	end,
	zfor_sup:start_link(ConfPath). 	% 返回{ok, Pid}/{error, Reason}

stop(_State)->
	ok.

