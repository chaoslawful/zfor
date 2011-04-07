-module(zfor_util_SUITE).
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
all() -> [cs1_test, cs_seq_test, cs_cplx_test, pmap_test].

% 单元测试
cs1_test(_Config) ->
	[] = zfor_util:consult_string("xx"),
	[xx] = zfor_util:consult_string("xx."),
	[xx] = zfor_util:consult_string("xx.\n"),
	[xx, y] = zfor_util:consult_string("xx.\ny.").

cs_seq_test(_Config) ->
	[a,b] = zfor_util:consult_string("a.\nb.\n").

cs_cplx_test(_Config) ->
	[
		{config_url, "a.b.c"},
		{host, [1, 2, 3, 4]}
	] = zfor_util:consult_string("{config_url, \"a.b.c\"}.\n{host,[1,2,3,4]}.\n").

pmap_test(_Config)->
	Ts = now(),
	zfor_util:pmap_timeout(
			fun (_) -> zfor_util:sleep(2000) end,
			lists:seq(1,100),
			1000
		),
	Te = now(),
	Tdiff = timer:now_diff(Te, Ts) / 1000.0,
	true = (Tdiff < 1100.0).

