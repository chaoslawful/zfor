% ZFOR Supervisor
-module(zfor_sup).
-include("zfor_common.hrl").
-behaviour(supervisor).
-export([start_link/1, init/1]).
%-compile([debug_info,export_all]).

start_link(ConfPath)->
	supervisor:start_link({local,?ZFOR_SUPERVISOR_SRVNAME},?MODULE,ConfPath).

init(ConfPath)->
	{ok,{
			% 重启策略为one_for_one，即哪个服务死掉就重启对应的服务
			{one_for_one,?MAX_RESTART_TIMES,?RESTART_STATS_TIMESPAN},
			[
				zfor_main_childspec(ConfPath) 		% 监控zfor_main服务
			]
		}
	}.

zfor_main_childspec(ConfPath)->
	{
		zfor_main, 					% Child ID: term()
		{zfor_main, start_link, [ConfPath]}, 	% Startup function: {M, F, A}, M=F=atom(), A=[term()]
		permanent, 						% Restart strategy: permanent|transient|temporary
		500, 							% Shutdown strategy: brutal_kill|integer()>=0(ms)|infinity
		worker, 						% Child process type: worker|supervisor
		[zfor_main] 					% Modules can be code-replaced: [M] for supervisor/gen_server/gen_fsm;
										% 								'dynamic' for gen_event.
	}.

% vim:ft=erlang ts=4 sw=4

