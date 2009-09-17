-module(zfor_main_SUITE).
-compile([debug_info,export_all]).
-include("ct.hrl").
-include("zfor_common.hrl").

init_per_suite(Config) ->
	os:cmd("rm -rf /tmp/zfor_conf_test"),
	file:make_dir("/tmp/zfor_conf_test"),
	file:write_file(
		"/tmp/zfor_conf_test/a.conf",
"
{global,
	[
		{config_url,\"http://localhost/test.conf\"},
		{config_ttl,3000},
		{resolve_timeout,1000},
		{server_port,1117}
	]
}.
{vhost, \"hello.com\",
	[
		{host, [\"localhost\",\"www.baidu.com\"]},
		{select_method, all_active},
		{check_ttl, 1000},
		{check_type, http},
		{http_path, \"/index.html\"},
		{check_timeout, 500},
		{group_threshold, 10}
	]
}.
"
	),
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
all() -> [main_test].

main_test(_Config) ->
	zfor_main:start_link("/tmp/zfor_conf_test"),
	zfor_main:stop().

