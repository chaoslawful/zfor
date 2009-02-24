-module(zfor_util_tests).
-include("zfor_common.hrl").
-compile([debug_info,export_all]).

disable_log_test() ->
	error_logger:tty(false).

cs1_test()->
	T1=zfor_util:consult_string("xx"),
	?assert(T1=:=[]),
	T2=zfor_util:consult_string("xx."),
	?assert(T2=:=[xx]),
	T3=zfor_util:consult_string("xx.\n"),
	?assert(T3=:=[xx]),
	T4=zfor_util:consult_string("xx.\ny."),
	?assert(T4=:=[xx,y]).

cs_seq_test()->
	T=zfor_util:consult_string("a.\nb.\n"),
	?assert(T=:=[a,b]).

cs_cplx_test()->
	T=zfor_util:consult_string("{config_url, \"a.b.c\"}.\n{host,[1,2,3,4]}.\n"),
	?assert(T=:=[{config_url,"a.b.c"},{host,[1,2,3,4]}]).

pmap_test()->
	Ts=now(),
	zfor_util:pmap_timeout(
			fun (_)->zfor_util:sleep(2000) end,
			lists:seq(1,100),
			1000
		),
	Te=now(),
	Tdiff=timer:now_diff(Te,Ts)/1000.0,
	?assert(Tdiff<1100.0).

