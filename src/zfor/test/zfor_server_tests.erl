-module(zfor_server_tests).
-include("zfor_common.hrl").
-compile([debug_info,export_all]).

disable_log_test() ->
	error_logger:tty(false).

convert_test() ->
	IPs=[{127,0,0,1},{192,168,1,2}],
	Exp= <<127,0,0,1,192,168,1,2>>,
	?assert(zfor_server:convert_addrs_to_binary(IPs)=:=Exp).

