% ZFOR配置数据处理函数
-module(zfor_config).
-include("zfor_common.hrl").
-export([reload_conf/1, set_conf_path/2, get_global_conf/1, get_vhost_conf/2, get_all_vhost_conf/1]).
-compile([debug_info, bin_opt_info]).
%-compile([debug_info,export_all]).

% 检查本地和远程配置文件是否发生变动，若有变化则重新载入所有本地和远程配置文件
% 返回值：{true / false, record(server_state)}
reload_conf(State) ->
	scan_and_reload_conf(State).

% 设置新的本地配置文件目录
% 返回值：record(server_state)
set_conf_path(State,ConfPath) ->
	State#server_state{conf_path=ConfPath}.

% 获取全局配置记录
% 返回值：record(global_conf)
get_global_conf(State) ->
	Tid=State#server_state.conf_ets_id,
	case ets:lookup(Tid,?GLOBAL_CONFIG_KEY) of
		[{?GLOBAL_CONFIG_KEY,_UpdateTS,GlobalConf}] -> GlobalConf;
		[] -> #global_conf{}
	end.

% 获取给定的虚拟主机配置记录
% 返回值：{ok, integer(), record(vhost_conf)} / undefined
get_vhost_conf(State,VHostname) ->
	Tid=State#server_state.conf_ets_id,
	VHostKey={?VHOST_CONFIG_PREFIX,VHostname},
	case ets:lookup(Tid,VHostKey) of
		[{VHostKey,UpdateTS,VHostConf}] -> {ok,UpdateTS,VHostConf};
		[] -> undefined
	end.

% 获取所有虚拟主机配置记录
% 返回值：{ok, [{VHostConfKey, integer(), record(vhost_conf)}]} / undefined
get_all_vhost_conf(State) ->
	Tid=State#server_state.conf_ets_id,
	case ets:match_object(Tid,{{?VHOST_CONFIG_PREFIX,'_'},'_','_'}) of
		[] -> undefined;
		Objs -> {ok,Objs}
	end.

% ------ 内部实现函数 -------

% 检查本地和远程配置文件是否被更改，若有更改则重新载入所有本地和远程配置文件
scan_and_reload_conf(State) ->
	LocalConfMod=scan_conf('local',State), 	% 扫描本地配置文件内容是否有变化
	RemoteConfMod=scan_conf('remote',State), 	% 扫描远程配置文件内容是否有变化
	{UpdatedFlag, NewState} = if
		LocalConfMod orelse RemoteConfMod ->
			% 本地或远程配置文件内容有变化,重新载入所有配置数据并改动配置数据更新时戳
			{true, reload_all_conf(State)};
		true ->
			% 本地和远程配置文件内容都没有变化,维持原有状态不变
			{false, State}
	end,
	% 获取所有预定义的远程主机列表内容，并将虚拟主机配置项中使用主机列表名的部分替换为对应的实际主机列表
	NewState1 = zfor_hostlist:replace_host_list(NewState),
	FinState = if
		UpdatedFlag =:= true ->
			temp_replace_ets(NewState1); 	% 用内部状态中的临时数据更新ETS表，并更改内部状态中的配置数据更新时戳
		true ->
			NewState1
	end,
	{UpdatedFlag, FinState}.

% 扫描本地配置文件目录内容,根据更改时戳和inode校验和判断目录内容是否发生了变化
scan_conf('local',State) ->
	LocalConfPath=State#server_state.conf_path, 	% 获得本地配置文件目录
	LastUpdate=State#server_state.conf_last_update, 	% 获得最近一次配置数据更新时戳
	LastConfChecksum=State#server_state.conf_last_checksum, 	% 获得最近一次配置文件目录内容inode校验和
	LocalResolvPath=State#server_state.resolv_path, 	% 获得系统DNS配置文件路径
	LastResolvChecksum=State#server_state.resolv_last_checksum, 	% 获得最近一次系统DNS配置文件inode校验和
	% 查看配置目录中是否有最近修改时戳变化的文件/目录，并计算所有文件/目录inode的校验和
	{ConfModified,ConfChecksum}=scan_local_conf(LocalConfPath,LastUpdate),
	{ResolvModified,ResolvChecksum}=scan_local_conf(LocalResolvPath,LastUpdate),
	if
		ConfModified 	% 若本地配置文件目录或其中某个文件的修改时戳晚于配置数据更新时戳
		orelse ResolvModified 	% 或系统DNS配置文件的修改时戳晚于配置数据更新时戳
		orelse (ConfChecksum=/=LastConfChecksum) 	% 或配置目录/文件inode有所变化
		orelse (ResolvChecksum=/=LastResolvChecksum) -> 	% 或系统DNS配置文件inode有所变化，则认为配置文件有变动
			true;
		true -> 	% 本地配置文件目录及其中文件的修改时戳和inode均无变化,则认为配置文件没有变动
			false
	end;
% 扫描远程配置文件,根据远程服务器报告判断文件内容是否发生了变化
scan_conf('remote',State) ->
	Tid=State#server_state.conf_ets_id, 				% 获得保存配置数据的ETS表ID
	LastUpdate=State#server_state.conf_last_update, 	% 获得最近一次配置数据更新时戳
	case ets:lookup(Tid,?GLOBAL_CONFIG_KEY) of 	% 从ETS表中查找全局配置参数
		[{?GLOBAL_CONFIG_KEY,_,GlobalConf}] ->
			RemoteUrls=GlobalConf#global_conf.config_url, 	% 提取全局配置参数中的远程配置文件URL列表
			scan_remote_conf(RemoteUrls,LastUpdate); 	% 扫描远程配置文件是否发生变动
		_ -> 	% 保存配置数据的ETS表中没有发现全局配置参数，等同于远程配置文件无变化
			false
	end.

scan_remote_conf(Urls,LastUpdate) ->
	% 并发检查所有远程配置文件是否有更改
	Results=zfor_util:pmap_timeout(
			fun (Url) ->
				% 检查HTTP配置文件是否更改时还是用HTTP/1.1请求，以方便配置服务器使用虚拟主机
				http:request(
						'head',
						{Url, [
								{"User-Agent",?REMOTE_HTTP_USERAGENT},
								% 注意：rfc1123_date()会进行时区转换，因此参数是本地时间而非GMT时间！
								{"If-Modified-Since",httpd_util:rfc1123_date(LastUpdate)}
							]},
						[{timeout,?REMOTE_HTTP_TIMEOUT}, {relaxed,true}],
						[]
					)
			end,
			Urls,
			?REMOTE_HTTP_TIMEOUT
		),
	% 计算是否有被改变的远程文件
	lists:foldl(
		fun
			({ok, {ok, {{_,200,_},_,_}}},_) ->
				% 远程文件自上次检查后有更改
				true;
			(_,Modified) ->
				Modified
		end,
		false,
		Results
	).

scan_local_conf(ConfPath,LastUpdate) ->
	Ctx=crypto:md5_init(),
	{MTimeUpdated,NewCtx}=scan_local_conf(ConfPath,LastUpdate,Ctx),
	Checksum=crypto:md5_final(NewCtx),
	{MTimeUpdated,Checksum}.

scan_local_conf(ConfPath,LastUpdate,Ctx) ->
	case file:read_file_info(ConfPath) of
		{ok, #file_info{type=Type, inode=Inode, mtime=MTime}} ->
			NewCtx=crypto:md5_update(Ctx,term_to_binary(Inode)), 	% 用入口的inode更新校验和
			GregSec1=calendar:datetime_to_gregorian_seconds(LastUpdate),
			GregSec2=calendar:datetime_to_gregorian_seconds(MTime),
			case Type of
				regular -> 		% 给定路径对应普通文件
					IsConf=lists:suffix(".conf",ConfPath),
					if
						not IsConf ->
							% 跳过所有扩展名不是.conf的文件
							{false, Ctx};
						GregSec1<GregSec2 ->
							% 文件最近更改时戳晚于最近配置数据载入时戳,认为文件发生了变动
							{true, NewCtx};
						true ->
							% 文件最近更改时戳早于等于最近配置数据载入时戳,认为文件未发生变动
							{false, NewCtx}
					end;
				directory -> 	% 给定路径对应目录
					if
						GregSec1<GregSec2 -> 	% 目录最近更改时戳晚于最近配置数据载入时戳,认为目录发生了变动
							{true, NewCtx};
						true -> 	% 目录最近更改时戳早于等于最近配置数据载入时戳,需要遍历目录下内容来判定其是否有变动
							case file:list_dir(ConfPath) of
								{ok, Filenames} ->
									% 获得了给定目录下的所有入口路径(除了.和..以外,另外所有路径都以相对路径表达)
									% 将该列表转换为绝对路径后遍历之,寻找发生变动的文件或目录
									lists:foldl(
										fun
											(_,{true, InCtx}) ->
												{true, InCtx};
											(Path,{_, InCtx}) ->
												scan_local_conf(Path,LastUpdate,InCtx)
										end,
										{false,NewCtx},
										lists:map(
											fun (RelPath) -> filename:join(ConfPath,RelPath) end,
											Filenames
										)
									);
								{error, Reason}-> 	% 获取给定目录下内容出错,记录日志并忽略该目录
									?WARN_LOG("List directory ~p error: ~p~n",[ConfPath,file:format_error(Reason)]),
									{false, Ctx}
							end
					end;
				_Other -> 		% 给定路径对应其他文件系统入口,忽略之
					{false, Ctx}
			end;
		{error, Reason} -> 	% 获取给定路径对应入口信息时出错,记录日志并忽略该路径
			?WARN_LOG("Read file '~p' info error: ~p~n",[ConfPath,file:format_error(Reason)]),
			{false, Ctx}
	end.
	
% 读入本地配置目录中的所有配置文件以及指定的所有远程配置文件,将配置数据更新到ETS表中,并返回更新的内部状态
reload_all_conf(State) ->
	State1=reload_resolv_info(State),
	% 采取将所有配置数据读取合并到临时结构中，再用该结构更新ETS表的策略，以避免在更新数据的过程中
	% 让其他模块读取到前后不一致的配置数据。
	State2=temp_read_conf('local',State1), 	% 载入本地配置文件并将其内容合并到内部状态里，另外还会刷新inode校验和
	temp_read_conf('remote',State2). 	% 载入远程配置文件并将其内容合并到内部状态里

% 重新载入系统DNS配置文件内容
reload_resolv_info(State) ->
	ResolvPath=State#server_state.resolv_path,
	Ctx=crypto:md5_init(),
	NewCtx=reload_resolv_info(ResolvPath,Ctx),
	Checksum=crypto:md5_final(NewCtx),
	State#server_state{resolv_last_checksum=Checksum}.

reload_resolv_info(Path,Ctx) ->
	case file:read_file_info(Path) of
		{ok, #file_info{type=Type, inode=Inode}} ->
			case Type of
				regular ->
					NewCtx=crypto:md5_update(Ctx,term_to_binary(Inode)), 	% 用入口的inode更新校验和
					{ok, Content}=inet_parse:resolv(Path), 	% 解析系统DNS设置
					inet_db:del_ns(), 	% 清除inet模块现有的nameserver记录
					update_ns(Content), % 用新载入的系统DNS设置更新inet模块的nameserver记录
					NewCtx;
				_ ->
					Ctx
			end;
		{error, Reason} ->
			?WARN_LOG("Read resolve file '~p' info error: ~p~n",[Path,file:format_error(Reason)]),
			Ctx
	end.

% 更新inet模块的nameserver记录
update_ns([]) ->
	ok;
update_ns(Content) ->
	case lists:keytake(nameserver,1,Content) of
		{value,{nameserver,IP},Remains} ->
			inet_db:add_ns(IP),
			update_ns(Remains);
		false->
			ok
	end.

% 读入本地配置文件内容并合并到内部状态里
temp_read_conf('local',State) ->
	LocalConfPath=State#server_state.conf_path,
	read_local_conf(LocalConfPath,State);
% 读入可能的远程配置文件内容并合并到内部状态里
temp_read_conf('remote',State) ->
	TempDict=State#server_state.conf_temp_dict, 	% 获取内部状态中的临时配置数据结构
	case dict:find(?GLOBAL_CONFIG_KEY,TempDict) of
		{ok, Val} -> 	% 找到了全局配置参数
			% 获取远程配置文件URL列表
			RemoteUrls=Val#global_conf.config_url,
			read_remote_conf(RemoteUrls,State);
		error -> 	% 没有显式配置全局参数，维持原有内部状态不变
			State
	end.

% 读入给定本地配置目录中的内容，并对应更新内部状态
read_local_conf(ConfPath,State) ->
	Ctx=crypto:md5_init(),
	% 遍历给定的本地配置文件目录，读取解析配置文件内容并将数据合并到内部状态里，此外还会
	% 计算每个文件/目录的inode校验和
	{NewState,NewCtx}=read_local_conf(ConfPath,Ctx,State),
	Checksum=crypto:md5_final(NewCtx),
	% 更新内部状态中的配置目录inode校验和
	NewState#server_state{conf_last_checksum=Checksum}.

read_local_conf(ConfPath,Ctx,State) ->
	case file:read_file_info(ConfPath) of
		{ok, #file_info{type=Type, inode=Inode}} ->
			NewCtx=crypto:md5_update(Ctx,term_to_binary(Inode)), 	% 用入口的inode更新校验和
			case Type of
				regular -> 		% 给定路径对应普通文件
					IsConf=lists:suffix(".conf",ConfPath),
					if
						not IsConf ->
							% 跳过所有扩展名不是.conf的文件
							{State, Ctx};
						true ->
							TempDict=State#server_state.conf_temp_dict,
							Content=case file:consult(ConfPath) of
										{ok, Terms} -> Terms;
										{error, Reason} ->
											?WARN_LOG("Config file '~p' read error: ~p~n",
													[ConfPath, file:format_error(Reason)])
									end,
							% 解析配置文件内容并合并到内部状态里的临时字典里
							NewDict=parse_and_merge(Content,TempDict),
							NewState=State#server_state{conf_temp_dict=NewDict},
							{NewState,NewCtx}
					end;
				directory -> 	% 给定路径对应目录
					case file:list_dir(ConfPath) of
						{ok, Filenames} ->
							% 获得了给定目录下的所有入口路径(除了.和..以外,另外所有路径都以相对路径表达)
							% 将该列表转换为绝对路径后遍历载入其下的内容
							lists:foldl(
								fun
									(Path,{OldState,InCtx}) ->
										read_local_conf(Path,InCtx,OldState)
								end,
								{State,NewCtx},
								lists:map(
									fun (RelPath) -> filename:join(ConfPath,RelPath) end,
									Filenames
								)
							);
						{error, Reason} -> 	% 获取给定目录下内容出错,记录日志并忽略该目录
							?WARN_LOG("List directory '~p' error: ~p~n",[ConfPath,file:format_error(Reason)]),
							{State, Ctx}
					end;
				_Other-> 		% 给定路径对应其他文件系统入口,忽略之
					{State, Ctx}
			end;
		{error, Reason}-> 	% 获取给定路径对应入口信息时出错,记录日志并忽略该路径
			?WARN_LOG("Read file '~p' info error: ~p~n",[ConfPath,file:format_error(Reason)]),
			{State, Ctx}
	end.
	

% 载入给定URL对应的远程配置文件(HTTP方式)，读取解析其内容并将数据合并到内部状态里
read_remote_conf(Urls,State) ->
	% 并发获取每个远程配置文件的内容
	Results=zfor_util:pmap_timeout(
			fun (Url) ->
				% 读取HTTP配置文件的请求还是用HTTP/1.1，以方便配置服务器使用虚拟主机
				http:request(
					'get',
					{Url, [{"User-Agent",?REMOTE_HTTP_USERAGENT}]},
					[{timeout,?REMOTE_HTTP_TIMEOUT}, {relaxed,true}],
					[]
				)
			end,
			Urls,
			?REMOTE_HTTP_TIMEOUT
		),
	% 将每个远程文件抓取进程的结果合并到内部状态里
	lists:foldl(
		fun
			({ok, {ok, {{_,200,_},_,Body}}}, OldState) ->
				% 远程文件获取成功，解析其内容并合并到内部临时字典中
				TempDict=OldState#server_state.conf_temp_dict,
				NewDict=parse_and_merge(
							zfor_util:consult_string(Body),
							TempDict
						),
				OldState#server_state{conf_temp_dict=NewDict};
			(_, OldState) ->
				% 远程文件获取失败(非200响应或操作超时)，维持原状态
				OldState
		end,
		State,
		Results
	).

% 解析给定的配置项列表，并合并到字典结构里
parse_and_merge(Terms,Dict) ->
	parse_and_merge(Terms,Dict,Dict).

parse_and_merge([],_,CurDict) ->
	CurDict;
parse_and_merge([Term|Remains],OldDict,CurDict) ->
	case Term of
		{'global', KVList}=GConf when is_list(KVList) ->
			NewDict=merge_kvs(GConf,CurDict),
			parse_and_merge(Remains,OldDict,NewDict);
		{'vhost', _, KVList}=VConf when is_list(KVList) ->
			NewDict=merge_kvs(VConf,CurDict),
			parse_and_merge(Remains,OldDict,NewDict);
		_ ->
			?ERR_LOG("Unrecognizable config term: ~p~n",[Term]),
			OldDict
	end.

merge_kvs(ConfTerms,Dict) ->
	merge_kvs(ConfTerms,Dict,Dict).

% 解析处理全局配置项
merge_kvs({'global',[]},_,CurDict) ->
	CurDict;
merge_kvs({'global',[H|T]},OldDict,CurDict) ->
	% 获取当前临时字典里的全局配置记录，若不存在则构造一个默认记录
	GRec=case dict:find(?GLOBAL_CONFIG_KEY,CurDict) of
				{ok, Val} -> Val;
				error -> #global_conf{}
		end,
	% 解析给定的全局配置项，并构造新的临时记录
	NewGRec=case H of
				{'host_list', Name, {'fixed', Hosts} = Tuple}
					when is_list(Name), is_list(Hosts) ->
					% 固定主机列表定义
					HostList = GRec#global_conf.host_list,
					% 将固定主机列表信息加入到全局配置信息中
					NewHostList = case lists:keyfind(Name, 1, HostList) of
						false -> [{Name, Tuple} | HostList];
						_ -> HostList
					end,
					GRec#global_conf{host_list = NewHostList};
				{'host_list', Name, {'confsrv', {Cmd, DatId, GrpId, SrvHost}} = Tuple}
					when is_list(Name), is_list(Cmd), is_list(DatId), is_list(GrpId), is_list(SrvHost) ->
					% Taobao 远程配置 HTTP 服务提供的主机列表定义
					HostList = GRec#global_conf.host_list,
					% 将远程主机列表信息加入到全局配置信息中
					NewHostList = case lists:keyfind(Name, 1, HostList) of
						false -> [{Name, Tuple} | HostList];
						_ -> HostList
					end,
					GRec#global_conf{host_list = NewHostList};
				{'config_url', Url} when is_list(Url) ->
					ConfUrls = GRec#global_conf.config_url,
					NewConfUrls = case lists:member(Url, ConfUrls) of
						true -> ConfUrls;
						false -> [Url | ConfUrls]
					end,
					GRec#global_conf{config_url=NewConfUrls};
				{'config_ttl',TTL} when is_integer(TTL), TTL>0 ->
					GRec#global_conf{config_ttl=TTL};
				{'resolve_timeout',Timeout} when is_integer(Timeout), Timeout>0 ->
					GRec#global_conf{resolve_timeout=Timeout};
				{'server_port', Port} when is_integer(Port), Port>0, Port<65536 ->
					GRec#global_conf{server_port=Port};
				Else ->
					?ERR_LOG("Unrecognizable global config term: ~p~n",[Else]),
					error
			end,
	% 若当前全局配置项解析处理成功则继续处理剩余的配置项，否则就中断解析处理过程，维持原有字典内容不变
	case NewGRec of
		error -> OldDict;
		_ ->
			NewDict=dict:store(?GLOBAL_CONFIG_KEY,NewGRec,CurDict),
			merge_kvs({'global',T},OldDict,NewDict)
	end;
% 解析处理虚拟主机配置项
merge_kvs({'vhost',_,[]},_,CurDict) ->
	CurDict;
merge_kvs({'vhost',VHostName,[H|T]},OldDict,CurDict) ->
	VHostKey={?VHOST_CONFIG_PREFIX,VHostName},
	% 获取当前临时字典里的全局配置记录，若不存在则构造一个默认记录
	VRec=case dict:find(VHostKey,CurDict) of
				{ok, Val} -> Val;
				error -> #vhost_conf{}
		end,
	% 解析给定的虚拟主机配置项，并构造新的临时记录
	NewVRec=case H of
				{'host', Hostnames} when is_list(Hostnames)->
					% 找出不重复的主机名按顺序合并到虚拟主机配置项中
					NewHostnames=Hostnames -- VRec#vhost_conf.hostnames,
					Hosts=VRec#vhost_conf.hostnames ++ NewHostnames,
					VRec#vhost_conf{hostnames=Hosts};
				{'host', {'host_list', Name} = Tuple} when is_list(Name) ->
					% 使用预定义的主机列表，保留虚拟主机配置项不变，等待补全主机列表并进行2次扫描时填充为实际的列表
					VRec#vhost_conf{hostnames = Tuple};
				{'select_method', Method} when
					Method=:='fallback';
					Method=:='round_robin';
					Method=:='grp_rand';
					Method=:='grp_all';
					Method=:='min_rt';
					Method=:='single_active';
					Method=:='all_active' ->
					VRec#vhost_conf{select_method=Method};
				{'check_ttl', TTL} when is_integer(TTL), TTL>0 ->
					VRec#vhost_conf{check_ttl=TTL};
				{'check_type', Type} when
					Type=:=http;
					Type=:=tcp ->
					VRec#vhost_conf{check_type=Type};
				{'check_port', Port} when is_integer(Port), Port>0, Port<65536 ->
					VRec#vhost_conf{check_port=Port};
				{'http_path', Path} when is_list(Path) ->
					VRec#vhost_conf{http_path=Path};
				{'check_timeout', Timeout} when is_integer(Timeout), Timeout>0 ->
					VRec#vhost_conf{check_timeout=Timeout};
				{'expect_response', Resp} when is_binary(Resp) ->
					VRec#vhost_conf{expect_response=Resp};
				{'group_threshold', Threshold} when is_integer(Threshold), Threshold>0 ->
					VRec#vhost_conf{group_threshold=Threshold};	
				{'failure_response', FResp} when FResp=:='all';FResp=:='none' ->
					VRec#vhost_conf{failure_response=FResp};
				Else ->
					?ERR_LOG("Unrecognizable vhost config term: ~p~n",[Else]),
					error
			end,
	% 若当前虚拟主机配置项解析处理成功则继续处理剩余的配置项，否则就中断解析处理过程，维持原有字典内容不变
	case NewVRec of
		error -> OldDict;
		_ ->
			NewDict=dict:store(VHostKey,NewVRec,CurDict),
			merge_kvs({'vhost',VHostName,T},OldDict,NewDict)
	end.

% 将内部状态中的临时配置数据结构提交到ETS表中，并更新内部状态中的配置数据更新时戳
temp_replace_ets(State) ->
	Now=erlang:localtime(),
	Tid=State#server_state.conf_ets_id,
	TempDict=State#server_state.conf_temp_dict,
	% 用临时配置数据结构更新ETS表中的对应键值对
	dict:fold(
		fun (Key,Val,In) ->
			ets:insert(Tid,{Key,Now,Val}),
			In
		end,
		void,
		TempDict
	),
	% 删除ETS表中没有得到更新的老键值对
	remove_stale_records(Tid,Now),
	% 清除内部状态中的临时配置数据结构，并刷新最后更新时戳
	State#server_state{
		conf_temp_dict=dict:new(),
		conf_last_update=Now
	}.

% 遍历ETS配置数据表，删除所有更新时戳早于给定时戳的记录
remove_stale_records(Tid,TS) ->
	% 锁定ETS表状态，以维持遍历时的记录数据一致性
	ets:safe_fixtable(Tid,true),
	remove_stale_records(Tid,TS,ets:first(Tid)),
	% ETS表遍历结束，解除状态锁定
	ets:safe_fixtable(Tid,false),
	ok.

remove_stale_records(_Tid,_TS,'$end_of_table') ->
	% ETS表遍历完毕
	ok;
remove_stale_records(Tid,TS,Key) ->
	case ets:lookup(Tid,Key) of
		[{Key,UpdateTS,_Data}] when UpdateTS<TS ->
			% 当前记录的更新时戳早于给定时戳，说明数据已经过期，删除之
			ets:delete(Tid,Key);
		_Other ->
			void
	end,
	remove_stale_records(Tid,TS,ets:next(Tid,Key)).

% vim:ft=erlang ts=4 sw=4

