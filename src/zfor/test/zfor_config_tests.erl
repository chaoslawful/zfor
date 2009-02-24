-module(zfor_config_tests).
-include("zfor_common.hrl").
-compile([debug_info,export_all]).

disable_log_test()->
	error_logger:tty(false).

make_dir_test()->
	os:cmd("rm -rf /tmp/zfor_conf_test"),
	file:make_dir("/tmp/zfor_conf_test"),
	file:make_dir("/tmp/zfor_conf_test/a"),
	file:write_file(
		"/tmp/zfor_conf_test/a.conf",
"
{global,
	[
		{server_port,1117}
	]
}.
"
	),
	file:write_file(
		"/tmp/zfor_conf_test/b.conf",
"
{vhost,\"xx.com\",
	[
		{host,[\"x.com\"]},
		{select_method,fallback}
	]
}.
"
	),
	os:cmd("touch /tmp/zfor_conf_test/a/c.conf").

zfor_config_start_test()->
	crypto:start(),
	inets:start().

reload_conf_test()->
	State=#server_state{
		conf_path="/tmp/zfor_conf_test",
		conf_ets_id=ets:new(?ETS_TABLE_CONFIG,[set,protected,{keypos,1}]),
		health_ets_id=ets:new(?ETS_TABLE_HEALTH,[set,public,{keypos,1}]),
		conf_temp_dict=dict:new(),
		proc_dict=dict:new()
	},
	{true, State1}=zfor_config:reload_conf(State),
	{false, State2}=zfor_config:reload_conf(State1),
	zfor_util:sleep(1000),
	{false, State3}=zfor_config:reload_conf(State2),

	os:cmd("touch /tmp/zfor_conf_test"),
	{true, State4}=zfor_config:reload_conf(State3),
	{false, State5}=zfor_config:reload_conf(State4),
	zfor_util:sleep(1000),
	{false, State6}=zfor_config:reload_conf(State5),

	os:cmd("touch /tmp/zfor_conf_test/a/c.conf"),
	{true, State7}=zfor_config:reload_conf(State6),
	{false, State8}=zfor_config:reload_conf(State7),
	zfor_util:sleep(1000),
	{false, _}=zfor_config:reload_conf(State8).

api_test()->
	State=#server_state{
		conf_path="/tmp/zfor_conf_test",
		conf_ets_id=ets:new(?ETS_TABLE_CONFIG,[set,protected,{keypos,1}]),
		health_ets_id=ets:new(?ETS_TABLE_HEALTH,[set,public,{keypos,1}]),
		conf_temp_dict=dict:new(),
		proc_dict=dict:new()
	},
	{true, State1}=zfor_config:reload_conf(State),
	GConf=zfor_config:get_global_conf(State1),
	ExpGConf=#global_conf{server_port=1117},
	?assert(GConf=:=ExpGConf),
	{ok,UpdateTS,VConf}=zfor_config:get_vhost_conf(State1,"xx.com"),
	ExpVConf=#vhost_conf{hostnames=["x.com"],select_method=fallback},
	?assert(VConf=:=ExpVConf),
	{ok,VConfs}=zfor_config:get_all_vhost_conf(State1),
	ExpVConfs=[{{?VHOST_CONFIG_PREFIX,"xx.com"},UpdateTS,ExpVConf}],
	?assert(VConfs=:=ExpVConfs).

zfor_config_stop_test()->
	inets:stop(),
	crypto:stop().

