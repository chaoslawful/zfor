-module(zfor_main_tests).
-include("zfor_common.hrl").
-compile([debug_info,export_all]).

disable_log_test() ->
	error_logger:tty(false).

make_dir_test() ->
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
	).

zfor_main_start_test() ->
	crypto:start(),
	inets:start(),
	zfor_main:start_link("/tmp/zfor_conf_test").

zfor_main_stop_test() ->
	zfor_main:stop(),
	inets:stop(),
	crypto:stop().

