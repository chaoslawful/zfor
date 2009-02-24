-module(zfor_server).
-include("zfor_common.hrl").
-export([udp_server/2]).
%-compile([debug_info,export_all]).

% UDP查询服务，用于同libzfor进行交互
% 返回值：ok
udp_server(State,Port) ->
	case gen_udp:open(Port, [binary,{active,true}]) of
		{ok, Sock} ->
			udp_server_loop(State,Sock);
		{error, Reason} ->
			?ERR_LOG("gen_udp:open() error: ~p~n",[Reason]),
			ok
	end.

udp_server_loop(State,Sock) ->
	receive
		{udp, Sock, Host, Port, Bin} ->
			% 在独立进程中处理请求并反馈结果
			spawn(
				fun () ->
					BinReply=handle_req(State,Bin),
					gen_udp:send(Sock,Host,Port,BinReply)
				end
			);
		Other ->
			?WARN_LOG("Unrecognizable message received in udp_server_loop(): ~p~n",[Other])
	end,
	udp_server_loop(State,Sock).

handle_req(State,<<Type, Data/binary>>) ->
	case Type of
		?REQ_DNS ->
			% 正向DNS解析请求
			VHostname=erlang:binary_to_list(Data),
			case zfor_caretaker:get_vhost(State,VHostname) of
				{ok, #vhost_stat{ips=IPs}} ->
					% 在ETS健康状态表中找到了请求解析的主机记录
					Length=erlang:length(IPs),
					BinAddrs=convert_addrs_to_binary(IPs),
					<<Length,BinAddrs/binary>>;
				_ ->
					% 未在ETS健康状态表中找到对应记录
					<<0>>
			end;
		_ ->
			?WARN_LOG("Unknown request type: ~p~n",[Type]),
			<<0>>
	end.

% 将tuple list转换为big-endian octets，最多包含16个IP地址
convert_addrs_to_binary(IPs) ->
	convert_addrs_to_binary(IPs,[],0).

convert_addrs_to_binary([],L,_) ->
	erlang:list_to_binary(lists:reverse(L));
convert_addrs_to_binary(_,L,?ZFOR_MAX_HOSTADDRS) ->
	erlang:list_to_binary(lists:reverse(L));
convert_addrs_to_binary([{A,B,C,D}|T],L,N) ->
	convert_addrs_to_binary(T,[D,C,B,A|L],N+1).

