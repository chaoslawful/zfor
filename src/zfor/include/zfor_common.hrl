-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(DEFAULT_CONFIG_PATH,"/usr/local/etc/zfor/"). 	% 默认本地配置文件保存目录
-define(DEFAULT_RESOLV_PATH,"/etc/resolv.conf"). 	% 默认系统DNS配置文件放置路径
-define(REMOTE_HTTP_TIMEOUT,1000). 	% 远程获取配置文件的最大超时(ms)
-define(REMOTE_HTTP_USERAGENT,"ZFOR/1.0"). 	% 远程获取配置文件时使用的User-Agent标识串
-define(ETS_TABLE_CONFIG,'zfor_ets_config'). 	% 配置数据ETS表名称
-define(ETS_TABLE_HEALTH,'zfor_ets_health'). 	% 主机健康状态ETS表名称
-define(GLOBAL_CONFIG_KEY,'global_conf'). 	% 全局配置参数在ETS及内部状态临时结构中保存用的键名
-define(VHOST_CONFIG_PREFIX,'vhost_conf'). 		% 虚拟主机配置参数在ETS及内部状态临时结构中保存用的键名前缀
-define(VHOST_HEALTH_PREFIX,'vhost_stat'). 		% 虚拟主机健康状态在ETS中保存用的键名前缀
-define(VHOST_CURHOST_PREFIX,'vhost_curhost').	% 虚拟主机当前活动主机选择下标在ETS中保存用的键名前缀
-define(VHOST_CURHOST_POS, 2).	% 活动主机下标字段在ETS记录中的位置
-define(PROCDICT_TYPE_UDP_SERVER,'udp_server').
-define(PROCDICT_TYPE_VHOST_CHECKER,'vhost_checker').
-define(ZFOR_MAIN_SRVNAME,'zfor_main'). 	% ZFOR配置数据管理驱动进程本地注册名称
-define(ZFOR_SUPERVISOR_SRVNAME,'zfor_sup'). 	% ZFOR服务监控程序本地注册名称
-define(ZFOR_MAX_HOSTADDRS,16). 	% 单个域名解析得到的IPv4地址最多数量
-define(RESTART_STATS_TIMESPAN,60). 	% ZFOR服务监控程序统计重启次数的时间段长度(s)
-define(MAX_RESTART_TIMES,3). 		% ZFOR服务监控程序在统计时间段内允许的最大重启次数，若该时间段内重启次数大于此
									% 设置，则服务监控程序自动终止自身及其下监控的所有服务，以避免某些错误场合下服务
									% 连续重启。

-define(REQ_DNS, 0). 	% 正向DNS解析请求

-define(ERR_LOG,error_logger:error_msg).
-define(WARN_LOG,error_logger:warning_msg).
-define(INFO_LOG,error_logger:info_msg).

% 各个配置数据缺省值
-define(DEFAULT_HOST_LIST, []).
-define(DEFAULT_GLOBAL_CONFIG_URL, []).
-define(DEFAULT_GLOBAL_CONFIG_TTL, 5000).
-define(DEFAULT_GLOBAL_RESOLVE_TIMEOUT, 1000).
-define(DEFAULT_GLOBAL_SERVER_PORT, 1117).
-define(DEFAULT_VHOST_HOST, []).
-define(DEFAULT_VHOST_SELECT_METHOD, 'grp_rand').
-define(DEFAULT_VHOST_CHECK_TTL, 3000).
-define(DEFAULT_VHOST_CHECK_TYPE, 'http').
-define(DEFAULT_VHOST_CHECK_PORT, 80).
-define(DEFAULT_VHOST_HTTP_PATH, "/status.html").
-define(DEFAULT_VHOST_HTTP_METHOD, 'head').
-define(DEFAULT_VHOST_HTTP_HOST, undefined).
-define(DEFAULT_VHOST_CHECK_TIMEOUT, 2000).
-define(DEFAULT_VHOST_GROUP_THRESHOLD, 10).
-define(DEFAULT_VHOST_FAILURE_RESPONSE, 'none').

% ZFOR服务状态数据结构
-record(server_state,
	{
		proc_dict, 							% 保存当前启动的功能进程列表
		conf_temp_dict, 					% 配置数据解析过程中临时保存配置数据的字典
		conf_last_update={{1980,1,1},{0,0,0}}, 	% 最近配置数据更新时间(localtime)
		conf_last_checksum, 				% 最近配置目录下各个文件/目录的inode校验和
		conf_path=?DEFAULT_CONFIG_PATH, 	% 配置文件放置路径
		resolv_path=?DEFAULT_RESOLV_PATH, 	% 系统DNS配置文件放置路径
		resolv_last_checksum, 				% 系统DNS配置文件inode校验和
		conf_ets_id, 	% 保存ZFOR配置数据的ETS表ID
		health_ets_id 	% 保存虚拟主机健康检查数据的ETS表ID
	}
).

% 功能进程项记录
-record(proc_info,
	{
		ts, 	% 进程活动检查时戳。localtime()
		pid 	% 进程ID。pid()
	}
).

% 全局配置参数记录结构
-record(global_conf,
	{
		host_list = ?DEFAULT_HOST_LIST,		% 主机列表定义，类型[{string(), tuple()}]
		config_url=?DEFAULT_GLOBAL_CONFIG_URL, 		% 远程配置文件URL列表，类型[string()]
		config_ttl=?DEFAULT_GLOBAL_CONFIG_TTL, 		% 配置数据有效时长，类型integer，单位ms
		resolve_timeout=?DEFAULT_GLOBAL_RESOLVE_TIMEOUT, 	% 主机域名解析超时时长，类型integer，单位ms
		server_port=?DEFAULT_GLOBAL_SERVER_PORT 		% zfor服务监听端口(仅在服务启动时有效)，类型integer
	}
).

% 虚拟主机配置参数记录结构
-record(vhost_conf,
	{
		hostnames=?DEFAULT_VHOST_HOST, 				% 虚拟主机下属主机域名列表，类型[string()]
		select_method=?DEFAULT_VHOST_SELECT_METHOD, 		% 虚拟主机下属主机选择策略，类型atom()
		check_ttl=?DEFAULT_VHOST_CHECK_TTL, 			% 虚拟主机下属各个主机的健康状态有效时长，类型integer，单位ms
		check_type=?DEFAULT_VHOST_CHECK_TYPE, 		% 虚拟主机下属各个主机健康状态检查方式，类型atom()，取值为http/tcp
		check_port=?DEFAULT_VHOST_CHECK_PORT, 		% 虚拟主机下属各个主机健康状态连接端口，类型integer
		http_path=?DEFAULT_VHOST_HTTP_PATH, 			% HTTP健康检查方式使用的检查文件URI路径，类型string()
		http_method=?DEFAULT_VHOST_HTTP_METHOD,			% HTTP健康检查方式所用的请求方法，类型atom()
		http_host=?DEFAULT_VHOST_HTTP_HOST,				% HTTP健康检查方式时添加的Host头，类型string()
		check_timeout=?DEFAULT_VHOST_CHECK_TIMEOUT, 		% 主机健康状态检查操作超时时长，类型integer，单位ms
		expect_response, 	% 连接到主机以后期望的反馈数据，类型binary()
		failure_response=?DEFAULT_VHOST_FAILURE_RESPONSE, 	% 虚拟主机下属所有主机健康检查都失败时的解析策略，类型atom()
		group_threshold=?DEFAULT_VHOST_GROUP_THRESHOLD 	% 主机响应时间成组门限时长，类型integer，单位ms
	}
).

% 虚拟主机健康状态记录结构
-record(vhost_stat,
	{
		state, 		% 虚拟主机最近一次综合健康检查状态，类型atom()，取值为alive/dead
		ips, 		% 虚拟主机在最近一次综合健康检查中根据自己配置的主机选择策略决定的IPv4地址，类型[tuple()]
		host_stats 	% 虚拟主机下属各个实际主机最近一次综合健康检查状态，类型[record(host_stat)]
	}
).

% 实际主机健康状态记录结构
-record(host_stat,
	{
		hostname, 	% 虚拟主机下属实际主机域名，类型string()
		state, 		% 虚拟主机下属实际主机最近一次健康检查状态，类型atom()，取值为alive/dead
		ip, 		% 虚拟主机下属实际主机最近一次健康检查解析出的IPv4地址，类型tuple()
		rt 			% 虚拟主机下属实际主机最近一次健康检查的总响应时间(ms)，类型integer()
	}
).

% vim:ft=erlang ts=4 sw=4

