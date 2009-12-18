-module(zfor_client_SUITE).
-compile([debug_info,export_all]).
-include("ct.hrl").
-include("zfor_client.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

% 所有单元测试函数
all() -> [basic_test, more_test, fail_test, vconf_test].

% 单元测试
basic_test(_Config) ->
	Ctx = zfor_client:context(?DEFAULT_ZFOR_SERVER, ?DEFAULT_ZFOR_PORT, ?DEFAULT_ZFOR_TIMEOUT),
	{'ok', Addr} = zfor_client:getaddr(Ctx, "127.0.0.1"),
	io:format("Resolved address for 127.0.0.1: ~p~n", [Addr]),
	{127, 0, 0, 1} = Addr,
	{'ok', Addrs} = zfor_client:getaddrs(Ctx, "127.0.0.1"),
	io:format("Resolved addresses for 127.0.0.1: ~p~n", [Addrs]),
	[{127, 0, 0, 1}] = Addrs,
	ok.

more_test(_Config) ->
	Ctx = zfor_client:context(?DEFAULT_ZFOR_SERVER, ?DEFAULT_ZFOR_PORT, ?DEFAULT_ZFOR_TIMEOUT),
	{'ok', Addr1} = zfor_client:getaddr(Ctx, "test.zfor"),
	io:format("Resolved address for test.zfor: ~p~n", [Addr1]),
	{127, 0, 0, 1} = Addr1,
	{'ok', Addrs1} = zfor_client:getaddrs(Ctx, "test.zfor"),
	io:format("Resolved addresses for test.zfor: ~p~n", [Addrs1]),
	[{127, 0, 0, 1}, {127, 0, 0, 2}] = Addrs1,
	{'ok', Addr2} = zfor_client:getaddr(Ctx, "www.baidu.com"),
	io:format("Resolved address for www.baidu.com: ~p~n", [Addr2]),
	{'ok', Addrs2} = zfor_client:getaddrs(Ctx, "www.google.com"),
	io:format("Resolved addresses for www.google.com: ~p~n", [Addrs2]),
	ok.

fail_test(_Config) ->
	Ctx1 = zfor_client:context(?DEFAULT_ZFOR_SERVER, 9876, ?DEFAULT_ZFOR_TIMEOUT),
	{'error', _} = zfor_client:getaddr(Ctx1, "test1.zfor"),
	{'error', _} = zfor_client:getaddrs(Ctx1, "test1.zfor"),
	Ctx2 = zfor_client:context(?DEFAULT_ZFOR_SERVER, ?DEFAULT_ZFOR_PORT, ?DEFAULT_ZFOR_TIMEOUT),
	{'error', _} = zfor_client:getaddr(Ctx2, "test1.zfor"),
	{'error', _} = zfor_client:getaddrs(Ctx2, "test1.zfor"),
	ok.

vconf_test(_Config) ->
	Ctx = zfor_client:context(?DEFAULT_ZFOR_SERVER, ?DEFAULT_ZFOR_PORT, ?DEFAULT_ZFOR_TIMEOUT),
	{'ok', "[\"127.0.0.1\",\"127.0.0.2\"]"} = zfor_client:getvconf(Ctx, "test.zfor", host),
	{'ok', "\"all_active\""} = zfor_client:getvconf(Ctx, "test.zfor", select_method),
	{'ok', _} = zfor_client:getvconf(Ctx, "test.zfor", check_ttl),
	{'ok', "\"http\""} = zfor_client:getvconf(Ctx, "test.zfor", check_type),
	{'ok', "6789"} = zfor_client:getvconf(Ctx, "test.zfor", check_port),
	{'ok', "\"/\""} = zfor_client:getvconf(Ctx, "test.zfor", http_path),
	{'ok', "\"head\""} = zfor_client:getvconf(Ctx, "test.zfor", http_method),
	{'ok', _} = zfor_client:getvconf(Ctx, "test.zfor", http_host),
	{'ok', _} = zfor_client:getvconf(Ctx, "test.zfor", check_timeout),
	{'ok', _} = zfor_client:getvconf(Ctx, "test.zfor", expect_response),
	{'ok', "\"all\""} = zfor_client:getvconf(Ctx, "test.zfor", failure_response),
	{'ok', _} = zfor_client:getvconf(Ctx, "test.zfor", group_threshold),
	{'error', _} = zfor_client:getvconf(Ctx, "test.zfor", xx),
	ok.

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

