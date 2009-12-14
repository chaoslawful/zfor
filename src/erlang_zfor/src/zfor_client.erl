-module(zfor_client).
-include("zfor_client.hrl").
-include("zfor_log.hrl").
-vsn("%VSN%").
-compile([debug_info, bin_opt_info]).
-export([context/3, getaddr/2, getaddrs/2]).

%% ====================== Exported API =======================

% @doc Make a new zfor client context from specified arguments.
% @spec context(Server :: string(), Port :: integer(), Timeout :: integer()) -> zfor_client_ctx()
% @end
% {{{
-spec context(Server :: string(), Port :: integer(), Timeout :: integer()) -> zfor_client_ctx().

context(Server, Port, Timeout) ->
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

getaddr(Ctx, Hostname) ->
	case getaddrs(Ctx, Hostname) of
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

getaddrs(Ctx, Hostname) ->
	case zfor_getaddrs(Ctx, Hostname) of
		{'ok', Addrs = [_ | _]} -> {'ok', Addrs};
		_ ->	% Not found in zfor server or error occured
			?INFO("Failed to resolving hostname (~p) through zfor, fall-back to inet~n", [Hostname]),
			case inet_getaddrs(Hostname) of
				{'ok', Addrs} -> {'ok', Addrs};
				{'error', Reason} -> {'error', Reason}
			end
	end.
% }}}

%% =================== Internal Functions ====================

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
	case gen_udp:open(0, [binary, {active, false}]) of
		{'ok', Sock} ->
			Req = make_zfor_request('DNS', [Hostname]),
			Server = Ctx#zfor_client_ctx.server,
			Port = Ctx#zfor_client_ctx.port,
			Timeout = Ctx#zfor_client_ctx.timeout,
			case gen_udp:send(Sock, Server, Port, Req) of
				'ok' ->
					case gen_udp:recv(Sock, 0, Timeout) of
						{'ok', {_, _, Reply}} ->
							parse_zfor_reply(Reply);
						{'error', Reason} ->
							?WARN("Failed to receive zfor reply: ~p~n", [Reason]),
							{'error', Reason}
					end;
				{'error', Reason} ->
					?WARN("Failed to send zfor request: ~p~n", [Reason]),
					{'error', Reason}
			end;
		{'error', Reason} ->
			?WARN("Failed to open UDP port: ~p~n", [Reason]),
			{'error', Reason}
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
	<<?REQ_DNS/integer, Bin/binary>>;

make_zfor_request(_, _) ->
	erlang:error('not_implemented').
% }}}

% @doc Parse zfor reply binary into IP lists.
% @spec parse_zfor_reply(Reply :: binary()) -> {'ok', AddrList :: [tuple()]} | {'error', Reason :: term()}
% @end
% {{{
-spec parse_zfor_reply(Reply :: binary()) -> {'ok', AddrList :: [tuple()]} | {'error', Reason :: term()}.

parse_zfor_reply(<<Len, Addrs/binary>>) ->
	parse_zfor_reply(Addrs, Len, []);

parse_zfor_reply(_) ->
	{'error', 'invalid_reply'}.
% }}}

% @doc Auxliary zfor reply parsing function.
% @spec parse_zfor_reply(Reply :: binary(), Cnt :: integer(), IPs :: [tuple()]) -> {'ok', [tuple()]} | {'error', term()}
% @end
% {{{
-spec parse_zfor_reply(Reply :: binary(), Cnt :: integer(), IPs :: [tuple()]) -> {'ok', [tuple()]} | {'error', term()}.

parse_zfor_reply(<<>>, 0, IPs) ->
	{'ok', lists:reverse(IPs)};

parse_zfor_reply(<<A, B, C, D, Remain/binary>>, Cnt, IPs) when Cnt > 0 ->
	parse_zfor_reply(Remain, Cnt - 1, [{A, B, C, D} | IPs]);

parse_zfor_reply(_, _, _) ->
	{'error', 'invalid_reply'}.
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

