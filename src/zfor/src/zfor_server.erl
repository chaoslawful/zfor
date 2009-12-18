-module(zfor_server).
-include("zfor_common.hrl").
-export([udp_server/2]).
-compile([debug_info, bin_opt_info, export_all]).

% @doc UDP service for interacting with client applications.
% @spec udp_server(State :: server_state(), Port :: integer()) -> 'ok'
% @end
% {{{
-spec udp_server(State :: server_state(), Port :: integer()) -> 'ok'.

udp_server(State,Port) ->
	case gen_udp:open(Port, [binary,{active,true}]) of
		{ok, Sock} ->
			udp_server_loop(State,Sock);
		{error, Reason} ->
			?ERR_LOG("gen_udp:open() error: ~p~n",[Reason]),
			ok
	end.
% }}}

% @doc Main loop for UDP service.
% @spec udp_server_loop(State :: server_state(), Sock :: port()) -> 'ok'
% @end
% {{{
-spec udp_server_loop(State :: server_state(), Sock :: port()) -> 'ok'.

udp_server_loop(State,Sock) ->
	receive
		{udp, Sock, Host, Port, Bin} ->
			% 在独立进程中处理请求并反馈结果
			spawn(
				fun () ->
					BinReply=handle_req(Bin,State),
					gen_udp:send(Sock,Host,Port,BinReply)
				end
			);
		Other ->
			?WARN_LOG("Unrecognizable message received in udp_server_loop(): ~p~n",[Other])
	end,
	udp_server_loop(State,Sock).
% }}}

% @doc Request Demultiplexer
% @spec handle_req(Packet :: binary(), State :: server_state()) -> binary()
% @end
% {{{
-spec handle_req(Packet :: binary(), State :: server_state()) -> binary().

handle_req(<<Type, Data/binary>>, State) ->
	case Type of
		?REQ_DNS ->
			handle_req(Data, 'req_dns', State);
		?REQ_GET_VCONF ->
			handle_req(Data, 'req_get_vconf', State);
		_ ->
			?WARN_LOG("Unknown request type: ~p~n", [Type]),
			<<0>>
	end.
% }}}

% @doc Request packet handler
% @spec handle_req(Data :: binary(), Cmd :: atom(), State :: server_state()) -> binary()
% @end
% {{{
-spec handle_req(Data :: binary(), Cmd :: atom(), State :: server_state()) -> binary().

% Processing hostname resolving request
handle_req(Data, 'req_dns', State) ->
	VHostname = erlang:binary_to_list(Data),
	case zfor_caretaker:get_vhost(State, VHostname) of
		{ok, #vhost_stat{ips = IPs}} ->	% Found the corresponding record in health status ETS table
			TotalLen = erlang:length(IPs),
			if	TotalLen >= 1 ->
					% There are more then one hosts alive
					case zfor_config:get_vhost_conf(State, VHostname) of
						{ok, _, #vhost_conf{select_method = 'round_robin'}} ->
							% Picking up a active host based on round-robin strategy
							% Updating current host index
							CurHostIdx = zfor_caretaker:inc_vhost_curhost(State, VHostname, 1, erlang:length(IPs)),
							RRIP = lists:nth(CurHostIdx, IPs),
							Length = 1,
							BinAddrs = convert_addrs_to_binary([RRIP]),
							<<Length, BinAddrs/binary>>;
						_ ->
							% Return result directly
							BinAddrs = convert_addrs_to_binary(IPs),
							<<TotalLen, BinAddrs/binary>>
					end;
				true ->
					% No alive hosts found
					<<0>>
			end;
		_ ->	% Failed to find the corresponding record in health status ETS table
			<<0>>
	end;

% Processing virtual hostname configuration retreiving request
handle_req(<<PropLen/integer, PropName:PropLen/binary, Data/binary>>, 'req_get_vconf', State) ->
	NameMap = [
		{<<"host">>, #vhost_conf.hostnames,
			fun	({'host_list', HLName}) -> list_to_binary(["{\"host_list\":\"", HLName, "\"}"]);
				(L) -> list_to_binary(io_lib:format("~p", [L]))
			end}
		, {<<"select_method">>, #vhost_conf.select_method,
			fun (T) -> list_to_binary([$", atom_to_list(T), $"]) end}
		, {<<"check_ttl">>, #vhost_conf.check_ttl,
			fun erlang:integer_to_list/1}
		, {<<"check_type">>, #vhost_conf.check_type,
			fun (T) -> list_to_binary([$", atom_to_list(T), $"]) end}
		, {<<"check_port">>, #vhost_conf.check_port,
			fun erlang:integer_to_list/1}
		, {<<"http_path">>, #vhost_conf.http_path,
			fun (T) -> list_to_binary([$", T, $"]) end}
		, {<<"http_method">>, #vhost_conf.http_method,
			fun (T) -> list_to_binary([$", atom_to_list(T), $"]) end}
		, {<<"http_host">>, #vhost_conf.http_host,
			fun (T) -> list_to_binary([$", T, $"]) end}
		, {<<"check_timeout">>, #vhost_conf.check_timeout,
			fun erlang:integer_to_list/1}
		, {<<"expect_response">>, #vhost_conf.expect_response,
			fun (T) -> list_to_binary([$", T, $"]) end}
		, {<<"failure_response">>, #vhost_conf.failure_response,
			fun (T) -> list_to_binary([$",atom_to_list(T),$"]) end}
		, {<<"group_threshold">>, #vhost_conf.group_threshold,
			fun erlang:integer_to_list/1}
	],
	VHostname = binary_to_list(Data),
	case zfor_config:get_vhost_conf(State, VHostname) of
		{'ok', _, VHostConf} ->
			case lists:keyfind(PropName, 1, NameMap) of
				{PropName, Idx, Serializer} ->
					Val = element(Idx, VHostConf),
					if	Val =:= 'undefined' -> <<1>>;
						true -> iolist_to_binary([1, Serializer(Val)])
					end;
				false ->
					?WARN_LOG("Unknown virtual hostname config property name: ~p~n", [PropName]),
					<<0>>
			end;
		'undefined' ->
			?WARN_LOG("Unable to get virtual hostname config properties~n"),
			<<0>>
	end;

handle_req(_, Req, _) ->
	?WARN_LOG("Unrecognized request command : ~p~n", [Req]),
	<<0>>.

% @doc Convert ipaddress() tuple list into big-endian octets.
% The result list could contains up to 16 addresses.
% @spec convert_addrs_to_binary(IPs :: [tuple()]) -> binary()
% @end
% {{{
-spec convert_addrs_to_binary(IPs :: [tuple()]) -> binary().

convert_addrs_to_binary(IPs) ->
	convert_addrs_to_binary(IPs, [], 0).
% }}}

% @doc Auxiliary function for converting ipaddress() tuple list into big-endian octets.
% @spec convert_addrs_to_binary(IPs :: [tuple()], Res :: [integer()], Num :: integer()) -> binary()
% @end
% {{{
-spec convert_addrs_to_binary(IPs :: [tuple()], Res :: [integer()], Num :: integer()) -> binary().

convert_addrs_to_binary([], L, _) ->
	erlang:list_to_binary(lists:reverse(L));

convert_addrs_to_binary(_, L, ?ZFOR_MAX_HOSTADDRS) ->
	erlang:list_to_binary(lists:reverse(L));

convert_addrs_to_binary([{A, B, C, D} | T], L, N) ->
	convert_addrs_to_binary(T, [D, C, B, A | L], N+1).
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

