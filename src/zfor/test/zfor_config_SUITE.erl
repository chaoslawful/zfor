-module(zfor_config_SUITE).
-compile([debug_info, export_all]).
-include("ct.hrl").
-include("zfor_common.hrl").

init_per_suite(Config) ->
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
	os:cmd("touch /tmp/zfor_conf_test/a/c.conf"),
	crypto:start(),
	inets:start(),
    Config.

end_per_suite(Config) ->
	inets:stop(),
	crypto:stop(),
	os:cmd("rm -rf /tmp/zfor_conf_test"),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

% 所有单元测试函数
all() -> [reload_conf_test, api_test].

% 单元测试
reload_conf_test(_Config) ->
	State = #server_state{
		conf_path = "/tmp/zfor_conf_test",
		conf_ets_id = ets:new(?ETS_TABLE_CONFIG, [set, protected, {keypos, 1}]),
		health_ets_id = ets:new(?ETS_TABLE_HEALTH, [set, public, {keypos, 1}]),
		conf_temp_dict = dict:new(),
		proc_dict  =dict:new()
	},
	{true, State1} = zfor_config:reload_conf(State),
	{false, State2} = zfor_config:reload_conf(State1),
	zfor_util:sleep(1000),
	{false, State3} = zfor_config:reload_conf(State2),

	os:cmd("touch /tmp/zfor_conf_test"),
	{true, State4} = zfor_config:reload_conf(State3),
	{false, State5} = zfor_config:reload_conf(State4),
	zfor_util:sleep(1000),
	{false, State6} = zfor_config:reload_conf(State5),

	os:cmd("touch /tmp/zfor_conf_test/a/c.conf"),
	{true, State7} = zfor_config:reload_conf(State6),
	{false, State8} = zfor_config:reload_conf(State7),
	zfor_util:sleep(1000),
	{false, _} = zfor_config:reload_conf(State8).

api_test(_Config) ->
	State = #server_state{
		conf_path = "/tmp/zfor_conf_test",
		conf_ets_id = ets:new(?ETS_TABLE_CONFIG, [set, protected, {keypos, 1}]),
		health_ets_id = ets:new(?ETS_TABLE_HEALTH, [set, public, {keypos, 1}]),
		conf_temp_dict = dict:new(),
		proc_dict = dict:new()
	},
	{true, State1} = zfor_config:reload_conf(State),
	#global_conf{server_port = 1117} = zfor_config:get_global_conf(State1),
	{ok, UpdateTS,
		#vhost_conf{hostnames = ["x.com"], select_method = fallback}} = zfor_config:get_vhost_conf(State1, "xx.com"),
	{ok, [{{?VHOST_CONFIG_PREFIX, "xx.com"}, UpdateTS, ExpVConf}]} = zfor_config:get_all_vhost_conf(State1).

