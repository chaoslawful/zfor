-module(zfor_server_SUITE).
-compile([debug_info,export_all]).
-include("ct.hrl").
-include("zfor_common.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

% 所有单元测试函数
all() -> [convert_test].

% 单元测试
convert_test(_Config) ->
	IPs  = [{127, 0, 0, 1}, {192, 168, 1, 2}],
	Exp = <<127, 0, 0, 1, 192, 168, 1, 2>>,
	Exp = zfor_server:convert_addrs_to_binary(IPs).

