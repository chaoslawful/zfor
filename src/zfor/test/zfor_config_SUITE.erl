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
		{server_port, 1117},
		{host_list, \"testhl\", {fixed, [\"127.0.0.1\", \"127.0.0.2\"]}},
		{host_list, \"testrm\", {confsrv, {\"getGroupData\",
											\"com.taobao.uic.userinfo.UserReadService:1.0.0\",
											\"HSF\",
											\"192.168.207.111:8080\"}}}
	]
}.
"
	),
	file:write_file(
		"/tmp/zfor_conf_test/b.conf",
"
{vhost, \"xx.com\",
	[
		{host, [\"x.com\"]},
		{select_method, fallback}
	]
}.
{vhost, \"yy.com\",
	[
		{host, {host_list, \"testhl\"}},
		{select_method, round_robin}
	]
}.
{vhost, \"zz.com\",
	[
		{host, {host_list, \"testrm\"}},
		{select_method, grp_all}
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
	{ok, VHosts} = zfor_config:get_all_vhost_conf(State1),
	3 = length(VHosts),
	{
		{?VHOST_CONFIG_PREFIX, "xx.com"},
		UpdateTS,
		#vhost_conf{hostnames = ["x.com"], select_method = fallback}
	} = lists:keyfind({?VHOST_CONFIG_PREFIX, "xx.com"}, 1, VHosts),
	{
		{?VHOST_CONFIG_PREFIX, "yy.com"},
		UpdateTS,
		#vhost_conf{hostnames = ["127.0.0.1", "127.0.0.2"], select_method = round_robin}
	} = lists:keyfind({?VHOST_CONFIG_PREFIX, "yy.com"}, 1, VHosts),
	{
		{?VHOST_CONFIG_PREFIX, "zz.com"},
		UpdateTS,
		#vhost_conf{hostnames = ["192.168.212.169"], select_method = grp_all}
	} = lists:keyfind({?VHOST_CONFIG_PREFIX, "zz.com"}, 1, VHosts).

