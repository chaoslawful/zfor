-module(zfor_main).
-include("zfor_common.hrl").
-export([start_link/1, stop/0, init/2, debug_output/3]).
-export([system_continue/3, system_terminate/4]).
%-compile([debug_info,export_all]).

start_link(ConfPath) ->
	proc_lib:start_link(?MODULE,init,[self(),ConfPath]).

stop() ->
	?ZFOR_MAIN_SRVNAME!stop,
	ok.

init(Parent,ConfPath) ->
	register(?ZFOR_MAIN_SRVNAME,self()),
	% 用当前时间初始化随机数发生器
	{A,B,C}=erlang:now(),
	random:seed(A,B,C),
	% 初始化服务器状态记录
	ConfTid=ets:new(?ETS_TABLE_CONFIG,[named_table,set,protected,{keypos,1}]),
	StatTid=ets:new(?ETS_TABLE_HEALTH,[named_table,set,public,{keypos,1}]),
	State=#server_state{
			conf_path=ConfPath,
			conf_ets_id=ConfTid,
			health_ets_id=StatTid,
			conf_temp_dict=dict:new(),
			proc_dict=dict:new()
		},
	DbgOpts=sys:debug_options([]),
	proc_lib:init_ack(Parent,{ok,self()}),
	loop(Parent,DbgOpts,State).

loop(Parent,DbgOpts,OldState)->
	DbgOpts2=sys:handle_debug(DbgOpts,{?MODULE,debug_output},?MODULE,{"prepare to reload configuration",OldState}),
	% 重新载入配置数据
	{Reloaded, State}=zfor_config:reload_conf(OldState),
	DbgOpts3=sys:handle_debug(DbgOpts2,{?MODULE,debug_output},?MODULE,
			if
				Reloaded -> {"configuration is refreshed",State};
				true -> {"configuration is NOT refreshed",State}
			end
		),
	% 读取全局配置参数
	GConf=zfor_config:get_global_conf(State),
	if
		Reloaded ->
			% 配置数据被重新载入，重置inet模块解析域名的超时时间
			inet_db:set_timeout(GConf#global_conf.resolve_timeout);
		true ->
			void
	end,
	DbgOpts4=sys:handle_debug(DbgOpts3,{?MODULE,debug_output},?MODULE,
			{
				"found global configuration",
				resolve_timeout, GConf#global_conf.resolve_timeout,
				config_ttl, GConf#global_conf.config_ttl,
				State
			}
		),

	Now=erlang:localtime(),
	% 用新的配置数据更新功能进程字典
	State1=update_procdict_by_conf(State,Now),
	% 检查功能进程字典中的各个项，重启尚未启动或死亡的进程，杀死无用进程并将其从字典中删除
	State2=supervise_procdict(State1,Now),

	% 休眠配置数据缓存有效期对应的时间
	Timeout=GConf#global_conf.config_ttl,
	DbgOpts5=sys:handle_debug(DbgOpts4,{?MODULE,debug_output},?MODULE,
			{
				"prepare to sleep",
				timeout, Timeout,
				State2
			}
		),
	receive
		'stop' ->
			sys:handle_debug(DbgOpts,{?MODULE,debug_output},?MODULE,"sleep broke by a stop message, exiting..."),
			ok;
		{system, From, Req} ->
			sys:handle_system_msg(Req,From,Parent,?MODULE,DbgOpts,State2)
	after Timeout->
		DbgOpts6=sys:handle_debug(DbgOpts5,{?MODULE,debug_output},?MODULE,"service awakening"),
		loop(Parent,DbgOpts6,State2)
	end.

% 根据全局和虚拟主机配置数据更新功能进程字典
update_procdict_by_conf(State, TS) ->
	GConf=zfor_config:get_global_conf(State),
	OldProcDict=State#server_state.proc_dict,
	
	% 更新功能进程字典中的UDP监听进程记录
	UDPProcKey={?PROCDICT_TYPE_UDP_SERVER,GConf#global_conf.server_port},
	ProcDict1=case dict:find(UDPProcKey,OldProcDict) of
				{ok, Entry} ->
					% 原功能进程字典中已记录对应端口的UDP监听服务，仅更新其检查时戳
					dict:store(UDPProcKey,Entry#proc_info{ts=TS},OldProcDict);
				_ ->
					% 原功能进程字典中没有记录对应端口的UDP监听服务，增加一个新记录
					dict:store(UDPProcKey,#proc_info{ts=TS},OldProcDict)
	end,
	
	% 从配置数据ETS表中找到所有虚拟主机，并更新功能进程字典中的对应项
	ProcDict2=ets:foldl(
		fun
			({{?VHOST_CONFIG_PREFIX,VHostname},_UpdateTS,_VHostConf}, OldDict) ->
				% 当前记录是虚拟主机配置项
				VHostProcKey={?PROCDICT_TYPE_VHOST_CHECKER,VHostname},
				% 根据该虚拟主机检查进程是否存在于字典中来决定是更新时戳还是新增记录
				case dict:find(VHostProcKey,OldDict) of
					{ok, PEntry} ->
						dict:store(VHostProcKey, PEntry#proc_info{ts=TS}, OldDict);
					_ ->
						dict:store(VHostProcKey, #proc_info{ts=TS}, OldDict)
				end;
			(_, OldDict) ->
				% 当前记录是其他数据
				OldDict
		end,
		ProcDict1,
		State#server_state.conf_ets_id
	),
	% 更新服务状态中的功能进程字典
	State#server_state{proc_dict=ProcDict2}.

% 遍历功能进程字典，检查所有已经更新的进程项，对于未启动或已经退出的项重新启动对应
% 类型的功能进程。对于未更新的进程项则强行杀死对应的进程并将其从字典中删除。
supervise_procdict(State, TS) ->
	OldProcDict=State#server_state.proc_dict,
	NewProcDict=dict:fold(
		fun
			(Key, #proc_info{ts=TS1,pid=Pid}=Val, OldDict) when TS1=:=TS ->
				IsAlive=if Pid=:=undefined -> false; true -> erlang:is_process_alive(Pid) end,
				if
					not IsAlive ->
						% 若当前项进程尚未启动，或进程已经死亡，则根据具体功能重新启动对应进程
						case Key of
							{?PROCDICT_TYPE_UDP_SERVER, Port} ->
								NPid=spawn(fun () -> zfor_server:udp_server(State, Port) end);
							{?PROCDICT_TYPE_VHOST_CHECKER, VHostname} ->
								NPid=spawn(fun () -> zfor_caretaker:vhost_checker(State, VHostname) end)
						end,
						% 用新进程ID更新当前项内容
						dict:store(Key, Val#proc_info{pid=NPid}, OldDict);
					true ->
						% 当前项对应进程处于活动状态，保持项内容不变
						dict:store(Key, Val, OldDict)
				end;
			({Type,Data}, #proc_info{pid=Pid}, OldDict)
				when Pid=/=undefined ->
				% 当前项检查时间没有更新，强制杀死对应进程，并将其从新字典中删除
				exit(Pid,kill),
				if
					Type=:=?PROCDICT_TYPE_VHOST_CHECKER ->
						% 若当前项对应虚拟主机健康检查进程，则从ETS健康状态表中删除该虚拟主机记录
						zfor_caretaker:del_vhost(State,Data);
					true ->
						void
				end,
				OldDict;
			(_, _, OldDict) ->
				% 将其他项从新字典中删除
				OldDict
		end,
		dict:new(),
		OldProcDict
	),
	% 更新服务状态中的功能进程字典
	State#server_state{proc_dict=NewProcDict}.

system_continue(Parent,DbgOpts,State) ->
	NewDbgOpts=sys:handle_debug(DbgOpts,{?MODULE,debug_output},?MODULE,
			"service continue after processing system message"),
	loop(Parent,NewDbgOpts,State).

system_terminate(Reason,_Parent,DbgOpts,State) ->
	sys:handle_debug(DbgOpts,{?MODULE,debug_output},?MODULE,{"service terminating",State}),
	stop_all_proc(State),
	exit(Reason).

debug_output(Dev,Event,Name) ->
	io:format(Dev,"* ~p event: ~p~n",[Name,Event]).

stop_all_proc(State) ->
	ProcDict=State#server_state.proc_dict,
	% 遍历功能进程字典，杀死所有已经派生的功能进程
	dict:fold(
		fun
			(_, #proc_info{pid=Pid}, Dummy)
				when Pid=/=undefined ->
				exit(Pid,kill),
				Dummy;
			(_, _, Dummy) -> Dummy
		end,
		void,
		ProcDict
	),
	ok.

