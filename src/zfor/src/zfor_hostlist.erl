-module(zfor_hostlist).
-export([replace_host_list/1]).
-compile([bin_opt_info]).
-include("zfor_common.hrl").

% 从服务器配置数据中获取定义的主机列表并更新用到了主机列表的虚拟主机配置项
% @spec replace_host_list(State::record(server_state)) -> record(server_state)
replace_host_list(State) when is_record(State, server_state) ->
	LookupDict = dump_host_list(State),
	NewState = replace_vhost(State, LookupDict),
	NewState.

% 根据给定的主机列表定义更新服务器配置数据中的虚拟主机配置项
replace_vhost(State, LookupDict) ->
	TempDict = State#server_state.conf_temp_dict,
	NewDict = dict:map(
		fun
			({?VHOST_CONFIG_PREFIX, VHostName}, OldVRec) ->	% 当前配置项为虚拟主机配置项
				case OldVRec#vhost_conf.hostnames of
					{'host_list', Name} ->	% 当前虚拟主机配置使用了预定义主机列表
						% 在预定义主机列表中查找对应的记录并替换对应的虚拟主机属性
						case dict:find(Name, LookupDict) of
							{'ok', Hosts} -> OldVRec#vhost_conf{hostnames = Hosts};
							'error' ->
								?WARN_LOG("Failed to find host list '~s' for virtual hostname '~s', use empty host list instaed!", [Name, VHostName]),
								OldVRec#vhost_conf{hostnames = []}
						end;
					_ ->	% 当前虚拟主机配置使用了显式列表
						OldVRec
				end;
			(_, OldVal) -> OldVal
		end,
		TempDict
	),
	State#server_state{conf_temp_dict = NewDict}.

% 将服务器配置数据中当前定义的主机列表导出为字典形式以便查找，并在必要时从远程获取主机列表
% @spec dump_host_list(State::record(server_state)) -> dict()
dump_host_list(State) ->
	TempDict = State#server_state.conf_temp_dict,
	GRec = case dict:find(?GLOBAL_CONFIG_KEY, TempDict) of
		{'ok', Val} -> Val;
		'error' -> #global_conf{}
	end,
	HostList = GRec#global_conf.host_list,
	dump_host_list(HostList, [], []).

dump_host_list([], FixHostList, RmtHostList) ->
	% 将固定主机列表合并到导出字典中
	ResDict = store_fixed_host_list_to_dict(FixHostList, dict:new()),
	% 并发获取所有远程主机列表
	Result = zfor_util:pmap_timeout(fun fetch_and_parse_host_list/1, RmtHostList, ?REMOTE_HTTP_TIMEOUT),
	% 将远程主机列表合并到导出字典中
	ResDict1 = store_result_to_dict(RmtHostList, Result, ResDict),
	ResDict1;
dump_host_list([{Name, {'fixed', Hosts}} | Rest], FixHostList, RmtHostList) ->
	dump_host_list(Rest, [{Name, Hosts} | FixHostList], RmtHostList);
dump_host_list([{Name, {'confsrv', _} = Tuple} | Rest], FixHostList, RmtHostList) ->
	dump_host_list(Rest, FixHostList, [{Name, Tuple} | RmtHostList]).

store_fixed_host_list_to_dict([], Dict) -> Dict;
store_fixed_host_list_to_dict([{Name, Hosts} | Rest], Dict) -> store_fixed_host_list_to_dict(Rest, dict:store(Name, Hosts, Dict)).

store_result_to_dict([], [], Dict) -> Dict;
store_result_to_dict([{Name, _} | R1], [{ok, Hosts} | R2], Dict) -> store_result_to_dict(R1, R2, dict:store(Name, Hosts, Dict));
store_result_to_dict([{Name, _} | R1], [{error, _} | R2], Dict) -> store_result_to_dict(R1, R2, dict:store(Name, [], Dict)).

% 解析淘宝 ConfigServer 主机列表
% 参考响应体：
%	192.168.212.169:12200?CLIENTRETRYCONNECTIONTIMES=3&CLIENTRETRYCONNECTIONTIMEOUT=1000&_SERIALIZETYPE=hessian&_IDLETIMEOUT=600&_TIMEOUT=3000\n
%	192.168.212.170:12200?CLIENTRETRYCONNECTIONTIMES=3&CLIENTRETRYCONNECTIONTIMEOUT=1000&_SERIALIZETYPE=hessian&_IDLETIMEOUT=600&_TIMEOUT=3000\n
%	192.168.212.171:12200?CLIENTRETRYCONNECTIONTIMES=3&CLIENTRETRYCONNECTIONTIMEOUT=1000&_SERIALIZETYPE=hessian&_IDLETIMEOUT=600&_TIMEOUT=3000
fetch_and_parse_host_list({_, {'confsrv', {Cmd, DatId, GrpId, SrvHost}}}) ->
	Url = lists:flatten(["http://", SrvHost, "/?command=", Cmd, "&dataId=", DatId, "&groupId=", GrpId]),
	{'ok', {{_, 200, _}, _, Body}} = http:request(
			'get',
			{Url, [{"User-Agent", ?REMOTE_HTTP_USERAGENT}]},
			[{timeout, ?REMOTE_HTTP_TIMEOUT}, {relaxed, true}],
			[]),
	Lines = string:tokens(Body, "\n"),
	Hosts = lists:map(fun (Line) ->
			HostAndPort = hd(string:tokens(Line, "?")),
			Host = hd(string:tokens(HostAndPort, ":")),
			Host
			end, Lines),
	Hosts.

