% ZFOR工具函数
-module(zfor_util).
-include("zfor_common.hrl").
-export([dump_config/0, dump_health/0, pmap_timeout/3, consult_string/1, sleep/1, waitfor/2]).
-compile([debug_info, bin_opt_info]).

% 美观形式显示当前使用的配置数据
-spec dump_config() -> 'ok'.
dump_config() ->
	Tid = ?ETS_TABLE_CONFIG,
	case ets:lookup(Tid, ?GLOBAL_CONFIG_KEY) of
		[{?GLOBAL_CONFIG_KEY, {{YY, MM, DD}, {H, M, S}}, GlobalConf}] ->
			io:format("Global config (Updated at ~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w)~n", [YY, MM, DD, H, M, S]),
			GConf = GlobalConf;
		[] ->
			io:format("No global config specified, use default values instead~n"),
			GConf = #global_conf{}
	end,
	io:format("Config TTL:\t\t~w ms~n", [GConf#global_conf.config_ttl]),
	io:format("Resolve Timeout:\t~w ms~n", [GConf#global_conf.resolve_timeout]),
	io:format("Server Port:\t\t~w~n", [GConf#global_conf.server_port]),
	URLs = GConf#global_conf.config_url,
	if
		erlang:length(URLs) > 0 ->
			io:format("Remote config URLs:~n"),
			lists:foreach(
				fun (Url) ->
					io:format("\t~s~n", [Url])
				end,
				URLs
			);
		true ->
			io:format("No remote config URLs~n")
	end,
	io:nl(),

	HostLists = GConf#global_conf.host_list,
	if
		erlang:length(HostLists) > 0 ->
			io:format("Predefined host lists:~n"),
			lists:foreach(
				fun ({Name, Tuple}) ->
					io:format("\t~s => ~p~n", [Name, Tuple])
				end,
				HostLists
			);
		true ->
			io:format("No predefined host lists~n")
	end,
	io:nl(),

	case ets:match_object(Tid, {{?VHOST_CONFIG_PREFIX, '_'}, '_', '_'}) of
		[] ->
			io:format("No virtual hostnames~n");
		VConfs ->
			dump_vhost_config(VConfs)
	end.

% 美观形式显示给定列表中的虚拟主机配置数据
-spec dump_vhost_config(VHostConfs::[vhost_conf_obj()]) -> 'ok'.
dump_vhost_config([]) -> ok;
dump_vhost_config([{{?VHOST_CONFIG_PREFIX, VHostname}, {{YY, MM, DD}, {H, M, S}}, VConf} | T]) ->
	io:format("Config for virtual hostname ~s (Updated at ~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w)~n",
			[VHostname, YY, MM, DD, H, M, S]),
	Hosts = VConf#vhost_conf.hostnames,
	if
		erlang:is_tuple(Hosts) ->
			% host list
			io:format("Hosts: predefined host list ~p~n", [Hosts]);
		erlang:length(Hosts) > 0 ->
			io:format("Hosts:~n"),
			lists:foreach(
				fun (Hostname) ->
					io:format("\t~s~n", [Hostname])
				end,
				Hosts
			);
		true ->
			io:format("\tHosts: none~n")
	end,
	io:format("Select method:\t\t~w~n", [VConf#vhost_conf.select_method]),
	io:format("Check interval:\t\t~w ms~n", [VConf#vhost_conf.check_ttl]),
	io:format("Check type:\t\t~w~n", [VConf#vhost_conf.check_type]),
	io:format("Check port:\t\t~w~n", [VConf#vhost_conf.check_port]),
	io:format("Check timeout:\t\t~w ms~n", [VConf#vhost_conf.check_timeout]),
	case VConf#vhost_conf.check_type of
		'http' ->
			io:format("HTTP method:\t\t~p~n", [VConf#vhost_conf.http_method]),
			io:format("HTTP path:\t\t~s~n", [VConf#vhost_conf.http_path]),
			io:format("HTTP host:\t\t~p~n", [VConf#vhost_conf.http_host]);
		'tcp' ->
			io:format("Expect response:\t~w~n", [VConf#vhost_conf.expect_response])
	end,
	io:format("Failure response:\t~w~n", [VConf#vhost_conf.failure_response]),
	io:format("Group threshold:\t~w ms~n", [VConf#vhost_conf.group_threshold]),
	io:nl(),
	dump_vhost_config(T).

% 美观形式显示当前保存的虚拟主机健康状态
-spec dump_health() -> 'ok'.
dump_health() ->
	Tid = ?ETS_TABLE_HEALTH,
	case ets:match_object(Tid, {{?VHOST_HEALTH_PREFIX, '_'}, '_', '_'}) of
		[] ->
			io:format("No virtual hostnames~n");
		VStats ->
			dump_vhost_health(VStats)
	end.

% 美观形式显示给定列表中的虚拟主机健康状态
-spec dump_vhost_health(VHostStats::[vhost_stat_obj()]) -> 'ok'.
dump_vhost_health([]) -> ok;
dump_vhost_health([{{?VHOST_HEALTH_PREFIX, VHostname}, {{YY, MM, DD}, {H, M, S}}, VStat} | T]) ->
	io:format("Health status for virtual hostname ~s (Updated at ~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w)~n",
			[VHostname, YY, MM, DD, H, M, S]),
	io:format("Status:\t\t~w~n",[VStat#vhost_stat.state]),
	IPs = VStat#vhost_stat.ips,
	if
		erlang:length(IPs) > 0 ->
			io:format("Resolved IPs:~n"),
			lists:foreach(
				fun (IP) ->
					io:format("\t~s~n", [inet_parse:ntoa(IP)])
				end,
				IPs
			);
		true ->
			io:format("Resolved IPs: Can't be resolved!~n")
	end,
	io:format("Host status:~n"),
	dump_host_health(VStat#vhost_stat.host_stats),
	io:nl(),
	dump_vhost_health(T).

% 美观形式显示给定实际主机的健康状态
-spec dump_host_health(HostStats::[host_stat()]) -> 'ok'.
dump_host_health([]) -> ok;
dump_host_health([#host_stat{hostname=Host, state=ST, ip=IP, rt=RT} | T]) ->
	io:format("\tHostname:\t~s~n", [Host]),
	io:format("\tStatus:\t\t~w~n", [ST]),
	if
		IP =/= undefined ->
			% 主机域名解析成功，显示IP
			io:format("\tResolved IP:\t~s~n", [inet_parse:ntoa(IP)]);
		true ->
			% 主机域名解析失败，不显示其IP
			void
	end,
	if
		ST =:= 'alive' ->
			% 主机通过了健康检查，显示测量到的响应时间
			io:format("\tResponse Time:\t~w ms~n", [RT]);
		true ->
			% 主机没有通过健康检查，没有响应时间可用
			void
	end,
	io:nl(),
	dump_host_health(T).

% 带超时时间的并行map操作，为原始列表中的每个元素派生一个独立进程执行给定的映射操作，并收集
% 各个映射操作的结果；在约定超时时间内完成的映射操作结果标记为{ok,Result}，未完成的映射操作
% 会被中断并标记对应的结果为{error,timeout}。返回的映射操作结果顺序同给定的原始列表一一对应。
-spec pmap_timeout(Fun::fun(), List::[term()], Timeout::integer()) -> [{'ok', term()} | {'error', 'timeout'}].
pmap_timeout(_, [], _) -> [];
pmap_timeout(Fun, List, Timeout) when
	is_function(Fun),
	is_integer(Timeout),
	Timeout > 0
->
	Parent = self(),
	Pairs = lists:map(
			fun (Arg) ->
				Tag = make_ref(),
				Pid = spawn(
					fun () ->
						Res = Fun(Arg),
						Parent ! {Tag, self(), Res}
					end
				),
				{Pid, Tag}
			end,
			List
		),
	% 顺序收集各个映射操作进程的结果
	{Result, _} = lists:foldl(
		fun ({Pid, Tag}, {Res, T}) ->
			Ts = now(),
			receive
				{Tag, Pid, Term} ->
					Te = now(),
					Tdiff = timer:now_diff(Te, Ts),
					Remain = T-Tdiff,
					{[{ok, Term} | Res], if Remain > 0 -> Remain; true -> 0 end}
			after trunc(T/1000) ->
				% 超时时间到，杀死未完成的映射操作进程
				exit(Pid, kill),
				% 映射操作进程可能在被杀死前发出了操作结果，这里检查一下
				% 是否有这种未处理的消息
				receive
					{Tag, Pid, Term} ->
						{[{ok, Term} | Res], 0}
				after 0 ->
					% 被杀死的映射操作进程确实没有返回结果消息，标记其结果为超时出错
					{[{error, timeout} | Res], 0}
				end
			end
		end,
		{[], Timeout*1000},
		Pairs
	),
	% 结果列表顺序同给定列表顺序相反，因此这里要颠倒一次结果列表的顺序
	lists:reverse(Result).

% 将给定字符串解析为Erlang Terms列表，因为不是解析文件所以没法用file:consult()
% 注意每个Erlang Term结束的.后面必须有一个空白符，否则会认为没有结束。
-spec consult_string(Str::string()) -> [term()].
consult_string(Str) ->
	consult_string([], Str++" ").

-spec consult_string(Terms::[term()], Remains::string()) -> [term()].
consult_string(Terms, "") ->
	lists:reverse(Terms);
consult_string(Terms, Remains)->
	case erl_scan:tokens([], Remains, 1) of
		{done, Result, NewRemains}->
			case Result of
				{ok, Tokens, _}->
					case erl_parse:parse_term(Tokens) of
						{ok, Term}->
							consult_string([Term|Terms], NewRemains);
						{error, {_, _, EDesc}}->
							?ERR_LOG("Unable to parse erlang term: ~p~n",
									   [erl_parse:format_error(EDesc)]),
							[]
					end;
				{eof, _}->
					lists:reverse(Terms);
				{error, EDesc, _}->
					?ERR_LOG("Unable to tokenize data: ~p~n",
							   [erl_scan:format_error(EDesc)]),
					[]
			end;
		{more, _}->
			lists:reverse(Terms)
	end.

-spec sleep(T::integer()) -> 'ok'.
sleep(T) ->
	receive
	after T -> ok
	end.

-spec waitfor(T::integer(), Msg::term()) -> {'ok', term()} | {'error', 'timeout'}.
waitfor(T,Msg) ->
	receive
		Msg -> {ok, Msg}
	after T -> {error, timeout}
	end.

