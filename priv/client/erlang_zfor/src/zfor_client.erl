-module(zfor_client).
-include("zfor_client.hrl").
-include("zfor_log.hrl").
-vsn("%VSN%").
-compile([debug_info, bin_opt_info]).
-export([context/3, getaddr/2, getaddrs/2, getaddr/3, getaddrs/3, getvconf/3]).

%% ====================== Exported API =======================

% @doc Make a new zfor client context from specified arguments.
% @spec context(Server :: string(), Port :: integer(), Timeout :: integer()) -> zfor_client_ctx()
% @end
% {{{
-spec context(Server :: string(), Port :: integer(), Timeout :: integer()) -> zfor_client_ctx().

context(Server, Port, Timeout) when is_list(Server), is_integer(Port), is_integer(Timeout) ->
	#zfor_client_ctx{
		server = Server,
		port = Port,
		timeout = Timeout
	}.
% }}}

% @doc Resolve the given hostname (real/virtual) through ZFOR service, return the 1st entry.
% @spec getaddr(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
%			{'ok', Address :: tuple()}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec getaddr(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
			{'ok', Address :: tuple()}
			| {'error', Reason :: term()}.

getaddr(Ctx, Hostname) when is_record(Ctx, zfor_client_ctx), is_list(Hostname) ->
	getaddr(Ctx, Hostname, true).
% }}}

% @doc Resolve the given hostname (real/virtual) through ZFOR service, return the 1st entry.
% (Permit to choose whether faill back to system-wide DNS resolving facility).
% @spec getaddr(Ctx :: zfor_client_ctx(), Hostname :: string(), Failback :: boolean()) ->
%			{'ok', Address :: tuple()}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec getaddr(Ctx :: zfor_client_ctx(), Hostname :: string(), Failback :: boolean()) ->
			{'ok', Address :: tuple()}
			| {'error', Reason :: term()}.

getaddr(Ctx, Hostname, Failback)
	when is_record(Ctx, zfor_client_ctx), is_list(Hostname), is_boolean(Failback) ->
	case getaddrs(Ctx, Hostname, Failback) of
		{'ok', [Addr | _]} -> {'ok', Addr};
		{'error', Reason} -> {'error', Reason}
	end.
% }}}

% @doc Resolve the given hostname (real/virtual) through ZFOR service, return all entries.
% @spec getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
%			{'ok', AddrList :: [tuple()]}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
			{'ok', AddrList :: [tuple()]}
			| {'error', Reason :: term()}.

getaddrs(Ctx, Hostname) when is_record(Ctx, zfor_client_ctx), is_list(Hostname) ->
	getaddrs(Ctx, Hostname, true).
% }}}

% @doc Resolve the given hostname (real/virtual) through ZFOR service, return all entries.
% (Permit to choose whether faill back to system-wide DNS resolving facility).
% @spec getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string(), Failback :: boolean()) ->
%			{'ok', AddrList :: [tuple()]}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string(), Failback :: boolean()) ->
			{'ok', AddrList :: [tuple()]}
			| {'error', Reason :: term()}.

getaddrs(Ctx, Hostname, Failback)
	when is_record(Ctx, zfor_client_ctx), is_list(Hostname), is_boolean(Failback) ->
	case zfor_getaddrs(Ctx, Hostname) of
		{'ok', Addrs = [_ | _]} -> {'ok', Addrs};
		Other ->	% Not found in zfor server or error occured
			case Failback of
				true ->		% Fail back permitted
					?INFO("Failed to resolving hostname (~p) through zfor, fall-back to inet~n", [Hostname]),
					case inet_getaddrs(Hostname) of
						{'ok', []} -> {'error', 'not_available'};
						{'ok', Addrs} -> {'ok', Addrs};
						{'error', Reason} -> {'error', Reason}
					end;
				false ->	% No fail back permitted
					case Other of
						{'ok', []} -> {'error', 'not_available'};
						{'error', Reason} -> {'error', Reason}
					end
			end
	end.
% }}}

% @doc Get config property for the given virtual host. The result is in JSON serialized form.
% @spec getvconf(Ctx :: zfor_client_ctx(), Hostname :: string(), Prop :: atom()) ->
%			{'ok', PropVal :: string()}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec getvconf(Ctx :: zfor_client_ctx(), Hostname :: string(), Prop :: atom()) ->
			{'ok', PropVal :: string()}
			| {'error', Reason :: term()}.

getvconf(Ctx, Hostname, Prop) when is_record(Ctx, zfor_client_ctx), is_list(Hostname), is_atom(Prop) ->
	zfor_getvconf(Ctx, Hostname, Prop).
% }}}

%% =================== Internal Functions ====================

% @doc Synchronize call ZFOR service
% @spec zfor_sync_call(Ctx :: zfor_client_ctx(), Req :: binary()) -> {'ok', binary()} | {'error', term()}
% @end
% {{{
-spec zfor_sync_call(Ctx :: zfor_client_ctx(), Req :: binary()) -> {'ok', binary()} | {'error', term()}.

zfor_sync_call(Ctx, Req) ->
	case gen_udp:open(0, [binary, {active, false}]) of
		{'ok', Sock} ->
			Server = Ctx#zfor_client_ctx.server,
			Port = Ctx#zfor_client_ctx.port,
			Timeout = Ctx#zfor_client_ctx.timeout,
			case gen_udp:send(Sock, Server, Port, Req) of
				'ok' ->
					case gen_udp:recv(Sock, 0, Timeout) of
						{'ok', {_, _, Reply}} -> {'ok', Reply};
						{'error', Reason} ->
							?WARN("Failed to receive zfor reply: ~p~n", [Reason]),
							{'error', Reason}
					end;
				{'error', Reason} ->
					?WARN("Failed to send zfor request: ~p~n", [Reason]),
					{'error', Reason}
			end
	end.
% }}}

% @doc Get virtual host config property using zfor service.
% @spec zfor_getvconf(Ctx :: zfor_client_ctx(), Hostname :: string(), Prop :: atom()) ->
%			{'ok', string()}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec zfor_getvconf(Ctx :: zfor_client_ctx(), Hostname :: string(), Prop :: atom()) ->
			{'ok', string()}
			| {'error', Reason :: term()}.

zfor_getvconf(Ctx, Hostname, Prop) ->
	Req = make_zfor_request('GET_VCONF', [Hostname, Prop]),
	case zfor_sync_call(Ctx, Req) of
		{'ok', Reply} -> parse_getvconf_reply(Reply);
		Other -> Other
	end.
% }}}

% @doc DNS resolving using zfor service.
% @spec zfor_getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
%			{'ok', [tuple()]}
%			| {'error', Reason :: term()}
% @end
% {{{
-spec zfor_getaddrs(Ctx :: zfor_client_ctx(), Hostname :: string()) ->
			{'ok', [tuple()]}
			| {'error', Reason :: term()}.

zfor_getaddrs(Ctx, Hostname) ->
	case inet_parse:ipv4_address(Hostname) of
		{'ok', Addr} -> {'ok', [Addr]};		% Numeric form IPv4 address
		_ ->	% Non-numeric form hostname, go on for actual resolving job
			Req = make_zfor_request('DNS', [Hostname]),
			case zfor_sync_call(Ctx, Req) of
				{'ok', Reply} -> parse_dns_reply(Reply);
				Other -> Other
			end
	end.
% }}}

% @doc Backup DNS resolving function using inet:getaddrs/2.
% @spec inet_getaddrs(Hostname :: string()) -> {'ok', [tuple()]} | {'error', Reason :: term()}
% @end
% {{{
-spec inet_getaddrs(Hostname :: string()) -> {'ok', [tuple()]} | {'error', Reason :: term()}.

inet_getaddrs(Hostname) ->
	case inet:getaddrs(Hostname, inet) of
		{'ok', Addrs} -> {'ok', Addrs};
		{'error', Reason} ->
			?WARN("Failed to resolve hostname (~p) by inet:getaddrs/2 : ~p~n", [Hostname, Reason]),
			{'error', Reason}
	end.
% }}}

% @doc Make a specified zfor request binary string.
% @spec make_zfor_request(Req :: atom(), Args :: [term()]) -> binary()
% @end
% {{{
-spec make_zfor_request(Req :: atom(), Args :: [term()]) -> binary().

make_zfor_request('DNS', [Hostname]) ->
	Bin = list_to_binary(Hostname),
	<<?ZFOR_REQ_DNS/integer, Bin/binary>>;

make_zfor_request('GET_VCONF', [Hostname, Prop]) ->
	PropBin = list_to_binary(atom_to_list(Prop)),
	PropLen = byte_size(PropBin),
	HostBin = list_to_binary(Hostname),
	<<?ZFOR_REQ_GET_VCONF/integer, PropLen/integer, PropBin:PropLen/binary, HostBin/binary>>;

make_zfor_request(_, _) ->
	erlang:error('not_implemented').
% }}}

% @doc Parse zfor reply binary into IP lists.
% @spec parse_dns_reply(Reply :: binary()) -> {'ok', AddrList :: [tuple()]} | {'error', Reason :: term()}
% @end
% {{{
-spec parse_dns_reply(Reply :: binary()) -> {'ok', AddrList :: [tuple()]} | {'error', Reason :: term()}.

parse_dns_reply(<<Len, Addrs/binary>>) ->
	parse_dns_reply(Addrs, Len, []);

parse_dns_reply(_) ->
	{'error', 'invalid_reply'}.
% }}}

% @doc Auxliary zfor reply parsing function.
% @spec parse_dns_reply(Reply :: binary(), Cnt :: integer(), IPs :: [tuple()]) -> {'ok', [tuple()]} | {'error', term()}
% @end
% {{{
-spec parse_dns_reply(Reply :: binary(), Cnt :: integer(), IPs :: [tuple()]) -> {'ok', [tuple()]} | {'error', term()}.

parse_dns_reply(<<>>, 0, IPs) ->
	{'ok', lists:reverse(IPs)};

parse_dns_reply(<<A, B, C, D, Remain/binary>>, Cnt, IPs) when Cnt > 0 ->
	parse_dns_reply(Remain, Cnt - 1, [{A, B, C, D} | IPs]);

parse_dns_reply(_, _, _) ->
	{'error', 'invalid_reply'}.
% }}}

% @doc Parse zfor reply binary for virtual host config property retreiving.
% @spec parse_getvconf_reply(Reply :: binary()) -> {'ok', string()} | {'error', Reason :: term()}
% @end
% {{{
-spec parse_getvconf_reply(Reply :: binary()) -> {'ok', string()} | {'error', Reason :: term()}.

parse_getvconf_reply(<<1, Val/binary>>) ->
	{'ok', binary_to_list(Val)};

parse_getvconf_reply(<<0>>) ->
	{'error', 'not_available'};

parse_getvconf_reply(_) ->
	{'error', 'invalid_reply'}.
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

