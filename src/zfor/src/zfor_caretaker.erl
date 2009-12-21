% ZFOR主机健康检查函数
% 功能：
% 	1. 检查当前配置数据中所有虚拟主机下属主机的健康状态，凡是主机域名解析超时、无法连接、连接
% 	后无主动握手数据包或握手数据包同期望数据串不符的主机都要标记为失效，每个主机的整个健康检查
% 	状态要受到健康检查超时的约束，对于超时仍未结束检查的主机标记为失效。对于有效的主机则记录其
% 	解析后的IP地址和响应时间(RT，可等同为整个连接过程的耗时)。
% 	2. 将虚拟主机下所有经检查有效的主机数据合并起来，根据配置参数决定的选择策略挑选出一个或多个
% 	条件相符的主机，用它们的数据构造出虚拟主机对应的正向域名解析记录保存到ETS表中。
-module(zfor_caretaker).
-include("zfor_common.hrl").
-export([
		del_vhost/2,
		get_vhost/2,
		refresh_status/2,
		vhost_checker/2,
		inc_vhost_curhost/3,
		inc_vhost_curhost/4,
		del_vhost_curhost/2
		]).
-compile([debug_info, bin_opt_info]).

%% ====================== Exported API =======================

% @doc Delete the health status record of specified virtual host.
% @spec del_vhost(State :: server_state(), VHostname : :string()) -> true
% @end
% {{{
-spec del_vhost(State::server_state(), VHostname::string()) -> true.

del_vhost(State, VHostname) ->
	HealthTid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_HEALTH_PREFIX, VHostname},
	ets:delete(HealthTid, VHostKey),
	del_vhost_curhost(State, VHostname).
% }}}

% @doc Get the health status record of specified virtual host.
% @spec get_vhost(State :: server_state(), VHostname :: string()) -> {'ok', vhost_stat()} | 'undefined'
% @end
% {{{
-spec get_vhost(State::server_state(), VHostname::string()) -> {'ok', vhost_stat()} | 'undefined'.

get_vhost(State, VHostname) ->
	Tid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_HEALTH_PREFIX, VHostname},
	case ets:lookup(Tid, VHostKey) of
		[{VHostKey, _, VHostStat}] ->
			{ok, VHostStat};
		_ ->
			undefined
	end.
% }}}

% @doc Force refreshing the health status of specified virtual host. (Blocking)
% @spec refresh_status(State :: server_state(), VHostname :: string()) -> boolean()
% @end
% {{{
-spec refresh_status(State::server_state(), VHostname::string()) -> boolean().

refresh_status(State, VHostname) ->
	case zfor_config:get_vhost_conf(State, VHostname) of
		undefined ->
			% 没有在配置数据ETS表中发现有效的虚拟主机设定
			false;
		{ok, _, VHostConf} ->
			% 对虚拟主机进行健康检查，并将检查结果更新到ETS表中
			check_and_update_vhost_stat(State, VHostname, VHostConf),
			true
	end.
% }}}

% @doc Virtual host health checker.
% It will periodly check and update the health status of specified virtual host
% according to corresponding config data. When the virtual host no longer existed
% in the config data, the checker will remove corresponding health status record
% and exit.
% @spec vhost_checker(State :: server_state(), VHostname :: string()) -> 'ok'
% @end
% {{{
-spec vhost_checker(State::server_state(), VHostname::string()) -> 'ok'.

vhost_checker(State, VHostname) ->
	case zfor_config:get_vhost_conf(State, VHostname) of
		{ok, _, VHostConf} ->
			% 对虚拟主机进行健康检查，并将检查结果更新到ETS表中
			check_and_update_vhost_stat(State, VHostname, VHostConf),
			% 基于虚拟主机健康检查状态有效时长休眠
			zfor_util:sleep(VHostConf#vhost_conf.check_ttl),
			vhost_checker(State, VHostname);
		_ ->
			% 无法在配置数据中找到给定虚拟主机对应的配置项，说明已经不需要该虚拟主机了
			% 删除当前虚拟主机对应的ETS健康状态记录并退出
			del_vhost(State, VHostname),
			ok
	end.
% }}}

% @doc Increase the curhost index of the specified virtual host, to ease implementing 'round_robin'
% load-balancing policy.
% @spec inc_vhost_curhost(State::server_state(), VHostname::string(), Step::integer()) -> integer()
% @end
% {{{
-spec inc_vhost_curhost(State::server_state(), VHostname::string(), Step::integer()) -> integer().

inc_vhost_curhost(State, VHostname, Step) ->
	HealthTid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_CURHOST_PREFIX, VHostname},
	case catch(ets:update_counter(HealthTid, VHostKey, {?VHOST_CURHOST_POS, Step})) of
		Result when erlang:is_integer(Result) ->
			% update curhost index successfully
			Result;
		_ ->
			% update curhost index failed, try to insert a new curhost record
			ets:insert_new(HealthTid, {VHostKey, 1}),
			1
	end.
% }}}

% @doc Increase/reset the curhost index of the specified virtual host, to ease implementing
% 'round_robin' load-balancing policy.
% @spec inc_vhost_curhost(
%			State :: server_state(),
%			VHostname :: string(),
%			Step :: integer(),
%			IdxLimit :: integer()
%		) -> integer()
% @end
% {{{
-spec inc_vhost_curhost(
	State :: server_state(),
	VHostname :: string(),
	Step :: integer(),
	IdxLimit :: integer()
) -> integer().

inc_vhost_curhost(State, VHostname, Step, IdxLimit) ->
	HealthTid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_CURHOST_PREFIX, VHostname},
	case catch(ets:update_counter(HealthTid, VHostKey, {?VHOST_CURHOST_POS, Step, IdxLimit, 1})) of
		Result when erlang:is_integer(Result) ->
			% update curhost index successfully
			Result;
		_ ->
			% update curhost index failed, try to insert a new curhost record
			ets:insert_new(HealthTid, {VHostKey, 1}),
			1
	end.
% }}}

% @doc Delete the curhost index record of the specified virtual host, to ease implementing 'round_robin'
% load-balancing policy.
% @spec del_vhost_curhost(State :: server_state(), VHostname :: string()) -> true
% @end
% {{{
-spec del_vhost_curhost(State::server_state(), VHostname::string()) -> true.

del_vhost_curhost(State, VHostname) ->
	HealthTid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_CURHOST_PREFIX, VHostname},
	ets:delete(HealthTid, VHostKey).
% }}}

%% =================== Internal Functions ====================

% @doc Check virtual host health status, and updating corresponding record in ETS table.
% @spec check_and_update_vhost_stat(
%			State :: server_state(),
%			VHostname :: string(),
%			VHostConf :: vhost_conf()
%		) -> 'ok'
% @end
% {{{
-spec check_and_update_vhost_stat(
	State :: server_state(),
	VHostname :: string(),
	VHostConf :: vhost_conf()
) -> 'ok'.

check_and_update_vhost_stat(State, VHostname, VHostConf) ->
	Hostnames = VHostConf#vhost_conf.hostnames,
	CheckTimeout = VHostConf#vhost_conf.check_timeout,
	% 并发检查给定虚拟主机下属所有实际主机的健康状态
	CheckResults = zfor_util:pmap_timeout(
			fun (Hostname) -> check_host_stat(Hostname, VHostConf) end,
			Hostnames,
			CheckTimeout
		),
	HostStats = lists:map(
			fun
				({{ok, HostStat}, _Hostname}) -> HostStat;
				({{error, _Reason}, Hostname}) ->
					#host_stat{
						hostname = Hostname,
						state = 'dead'
					}
			end,
			lists:zip(CheckResults, Hostnames)
		),
	% 根据实际主机检查结果和选择策略刷新ETS中的虚拟主机健康状态记录
	update_vhost_stat(State, VHostname, HostStats, VHostConf).
% }}}

% @doc Update virtual host health status record in ETS table.
% @spec update_vhost_stat(
%			State :: server_state(),
%			VHostname :: string(),
%			HostStats :: [host_stat()],
%			VHostConf :: vhost_conf()
%		) -> 'ok'
% @end
% {{{
-spec update_vhost_stat(
	State :: server_state(),
	VHostname :: string(),
	HostStats :: [host_stat()],
	VHostConf :: vhost_conf()
) -> 'ok'.

update_vhost_stat(State, VHostname, HostStats, VHostConf) ->
	Tid = State#server_state.health_ets_id,
	VHostKey = {?VHOST_HEALTH_PREFIX, VHostname},
	UpdateTS = erlang:localtime(),
	% 根据虚拟主机域名和其下实际主机的健康检查结果构造虚拟主机健康状态记录
	VHostState = make_vhost_stat(VHostname, HostStats, VHostConf),
	% 将虚拟主机健康检查结果插入ETS表
	ets:insert(Tid, {VHostKey, UpdateTS, VHostState}),
	ok.
% }}}

% @doc Construct virtual host health status record according to its real hosts'
% health checking results.
% @spec make_vhost_stat(
%			VHostname :: string(),
%			HostStats :: [host_stat()],
%			VHostConf :: vhost_conf()
%		) -> vhost_stat()
% @end
% {{{
-spec make_vhost_stat(
	VHostname :: string(),
	HostStats :: [host_stat()],
	VHostConf :: vhost_conf()
) -> vhost_stat().

make_vhost_stat(VHostname, HostStats, VHostConf) ->
	case VHostConf#vhost_conf.failure_response of
		'all' ->
			% 虚拟主机解析失败时返回其下所有主机地址。
			% 此时主机顺序很重要，因此结果列表需要反转一次以恢复同主机域名的对应关系
			AllHostIPs = lists:reverse(
				lists:foldl(
					fun
						(#host_stat{ip = undefined}, IPs) -> IPs;
						(#host_stat{ip = IP}, IPs) -> [IP | IPs]
					end,
					[],
					HostStats
				)
			),
			DeadVHost = #vhost_stat{state = 'dead', host_stats = HostStats, ips = AllHostIPs};
		'none' ->
			% 虚拟主机解析失败时不返回主机地址。
			DeadVHost = #vhost_stat{state = 'dead', host_stats = HostStats, ips = []}
	end,
	AliveVHost = #vhost_stat{state = 'alive', host_stats = HostStats},
	% 根据虚拟主机配置的主机选择策略分别进行处理
	case VHostConf#vhost_conf.select_method of
		'fallback' ->
			% 选择首个活动主机
			case lists:keysearch('alive', #host_stat.state, HostStats) of
				{value, #host_stat{ip = IP}} ->
					% 找到了活动主机，虚拟主机地址就是该主机地址
					AliveVHost#vhost_stat{ips = [IP]};
				false ->
					% 未找到活动主机，虚拟主机域名不可用
					DeadVHost
			end;	
		'single_active' ->
			% 选择仅有的一个活动主机，若多个主机同时活动则认为虚拟主机失效
			% 先找出所有活动主机
			ActiveHosts=lists:filter(
					fun
						(#host_stat{state = 'alive'}) -> true;
						(_) -> false
					end,
					HostStats
				),
			case ActiveHosts of
				[H] ->
					% 活动主机列表只有1个元素，虚拟主机地址就是这唯一活动主机的地址
					AliveVHost#vhost_stat{ips = [H#host_stat.ip]};
				_ ->
					% 没有活动主机，或活动主机多于1个，虚拟主机域名不可用
					DeadVHost
			end;
		'all_active' ->
			% 选择所有活动主机，不考虑RT差异
			% 找出所有活动主机的IPv4地址(这里主机先后顺序不重要，因此不需要反转结果列表)
			ActiveIPs=lists:foldl(
					fun
						(#host_stat{state = 'alive', ip = IP}, IPs) -> [IP | IPs];
						(_, IPs) -> IPs
					end,
					[],
					HostStats
				),
			case ActiveIPs of
				[] ->
					% 没有活动主机，虚拟主机域名不可用
					DeadVHost;
				_ ->
					% 有活动主机，虚拟主机地址列表就是所有活动主机的地址列表
					AliveVHost#vhost_stat{ips = ActiveIPs}
			end;
		'min_rt' ->
			% 选择RT最小的活动主机
			% 寻找虚拟主机下属的所有实际主机中处于活动状态且RT时间最小的主机
			{_, MinRTIP}=lists:foldl(
					fun
						(#host_stat{state = 'alive', ip = IP, rt = RT}, {GRT, _})
							when RT < GRT -> {RT, IP};
						(_, {GRT, GIP}) -> {GRT, GIP}
					end,
					{99999999, undefined},
					HostStats
				),
			case MinRTIP of
				undefined ->
					% 没有找到活动主机，虚拟主机域名不可用
					DeadVHost;
				_ ->
					% 找到了RT最小的活动主机，虚拟主机地址就是该主机地址
					AliveVHost#vhost_stat{ips = [MinRTIP]}
			end;
		'round_robin' ->
			% Pickup an active host based on round-robin strategy
			% NOTE: The actual round-robin picking up is proceeded in
			% zfor_server to achieve better balancing, here we only get
			% all active hosts.
			ActiveIPs=lists:foldl(
					fun
						(#host_stat{state = 'alive', ip = IP}, IPs) -> [IP | IPs];
						(_, IPs) -> IPs
					end,
					[],
					HostStats
				),
			case ActiveIPs of
				[] ->
					% 没有活动主机，虚拟主机域名不可用
					DeadVHost;
				_ ->
					% 有活动主机，虚拟主机地址列表就是所有活动主机的地址列表
					AliveVHost#vhost_stat{ips = ActiveIPs}
			end;
		'grp_rand' ->
			% 在RT差异小于预设门限的活动主机中随机挑选一个
			GrpThres = VHostConf#vhost_conf.group_threshold,
			% 获取所有活动主机
			ActiveHosts = lists:filter(
					fun
						(#host_stat{state = 'alive'}) -> true;
						(_) -> false
					end,
					HostStats
				),
			% 将活动主机按照RT升序排列
			SrtActiveHosts = lists:keysort(#host_stat.rt, ActiveHosts),
			case SrtActiveHosts of
				[H | T] ->
					% 有活动主机，根据RT成组门限时间选择活动主机
					GrpActiveHosts = [H | lists:takewhile(
							fun
								(#host_stat{rt = RT})
									when RT - H#host_stat.rt =< GrpThres -> true;
								(_) -> false
							end,
							T
						)],
					% 随机选择一个组内的活动主机
					RandGrpHost = lists:nth(random:uniform(erlang:length(GrpActiveHosts)), GrpActiveHosts),
					% 虚拟主机地址就是该主机地址
					AliveVHost#vhost_stat{ips = [RandGrpHost#host_stat.ip]};
				_ ->
					% 没有活动主机，虚拟主机域名不可用
					DeadVHost
			end;
		'grp_all' ->
			% 选择所有RT差异小于预设门限的活动主机
			GrpThres = VHostConf#vhost_conf.group_threshold,
			% 获取所有活动主机
			ActiveHosts = lists:filter(
					fun
						(#host_stat{state = 'alive'}) -> true;
						(_) -> false
					end,
					HostStats
				),
			% 将活动主机按照RT升序排列
			SrtActiveHosts = lists:keysort(#host_stat.rt, ActiveHosts),
			case SrtActiveHosts of
				[H | T] ->
					% 有活动主机，根据RT成组门限时间选择活动主机
					GrpActiveHosts = [H | lists:takewhile(
							fun
								(#host_stat{rt = RT})
									when RT - H#host_stat.rt =< GrpThres -> true;
								(_) -> false
							end,
							T
						)],
					GrpActiveHostIPs = lists:map(fun (#host_stat{ip = IP}) -> IP end, GrpActiveHosts),
					% 虚拟主机地址就是组内所有活动主机地址
					AliveVHost#vhost_stat{ips = GrpActiveHostIPs};
				_ ->
					% 没有活动主机，虚拟主机域名不可用
					DeadVHost
			end;
		Other ->
			% 未知主机选择方案，认为虚拟主机域名不可用
			?WARN_LOG("Unrecognizable host selection method '~p' for vhost ~p~n", [Other, VHostname]),
			DeadVHost
	end.
% }}}

% @doc Check specified real host health status.
% @spec check_host_stat(Hostname :: string(), VHostConf :: vhost_conf()) -> host_stat()
% @end
% {{{
-spec check_host_stat(Hostname :: string(), VHostConf :: vhost_conf()) -> host_stat().

check_host_stat(Hostname, VHostConf) ->
	DeadState = #host_stat{hostname = Hostname, state = 'dead'},
	% 1. 解析主机域名为IPv4地址
	case inet:getaddr(Hostname,inet) of
		{ok, IP} ->
			% 2. 根据主机健康检查方式选择不同的途径
			CheckPort = VHostConf#vhost_conf.check_port,
			AliveState = #host_stat{hostname = Hostname, state = 'alive', ip = IP},
			Ts = now(),
			case VHostConf#vhost_conf.check_type of
				'http' ->
					% 2a. HTTP健康检查方式
					Path = VHostConf#vhost_conf.http_path,
					URL = lists:flatten(["http://", inet_parse:ntoa(IP), ":", integer_to_list(CheckPort), Path]),
					Method = VHostConf#vhost_conf.http_method,
					TmpHeaders = [{"User-Agent", ?REMOTE_HTTP_USERAGENT}],
					HostHeader = VHostConf#vhost_conf.http_host,
					Headers = if
						HostHeader =/= undefined -> [{"Host", HostHeader} | TmpHeaders];
						true -> TmpHeaders
					end,
					Version = if
						HostHeader =/= undefined -> "HTTP/1.1";
						true -> "HTTP/1.0"
					end,
					% 尝试获取远程状态文件信息
					case http:request(
							Method,
							{URL, Headers},
							[{relaxed, true}, {version, Version}],
							[]
						) of
						{ok, {{_, Status, _}, _, _}} ->
							case Status of
								200 ->
									% 获取信息成功，计算耗时并返回成功记录
									Te = now(),
									Tdiff = erlang:trunc(timer:now_diff(Te, Ts) / 1000.0),
									AliveState#host_stat{rt = Tdiff};
								_ ->
									% 返回状态码无效
									?WARN_LOG("Abnormal response status code ~p for URL ~p~n", [Status, URL]),
									DeadState#host_stat{ip = IP}
							end;
						_ ->
							% 获取远程状态文件信息失败
							?WARN_LOG("Failed to http ~p file: URL is ~p~n", [Method, URL]),
							DeadState#host_stat{ip = IP}
					end;
				'tcp' ->
					% 2b. TCP健康检查方式
					case gen_tcp:connect(IP, CheckPort, [binary, {packet, 0}, {active, false}]) of
						{ok, Sock} ->
							% 成功建立TCP/IP连接
							ExpectResp = VHostConf#vhost_conf.expect_response,
							case ExpectResp of
								undefined ->
									% 未定义期望的初始握手包，认为健康检查通过
									Te = now(),
									Tdiff = erlang:trunc(timer:now_diff(Te, Ts) / 1000.0),
									Ret = AliveState#host_stat{rt = Tdiff};
								_ ->
									% 定义了期望的初始握手包，尝试读取握手包数据并校验
									Size = erlang:size(ExpectResp),
									case gen_tcp:recv(Sock, 0) of
										{ok, <<ExpectResp:Size/binary, _/binary>>} ->
											% 校验成功
											Te = now(),
											Tdiff = erlang:trunc(timer:now_diff(Te, Ts) / 1000.0),
											Ret = AliveState#host_stat{rt = Tdiff};
										{ok, Other} ->
											% 校验失败
											?WARN_LOG("Unexpected handshake packet received from ~p: ~p~n",
												[Hostname, Other]),
											Ret = DeadState#host_stat{ip = IP};
										_ ->
											% 读取初始握手包失败
											?WARN_LOG("Failed to read handshake packet from ~p~n", [Hostname]),
											Ret = DeadState#host_stat{ip = IP}
									end
							end,
							gen_tcp:close(Sock),
							Ret;
						_ ->
							% 无法建立TCP/IP连接
							?WARN_LOG("Failed to connect to ~p~n", [Hostname]),
							DeadState#host_stat{ip = IP}
					end;
				Other ->
					% 2c. 目前不支持的其他健康检查方式
					?WARN_LOG("Unknown host check type: ~p~n", [Other]),
					DeadState#host_stat{ip = IP}
			end;
		_ ->
			?WARN_LOG("Unable to resolve hostname ~p~n", [Hostname]),
			DeadState
	end.
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

