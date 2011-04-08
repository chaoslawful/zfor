-module(zfor_httpclient).
-include("zfor_httpclient.hrl").
-export([make_request/7, request/1]).
-compile([debug_info, bin_opt_info]).

%% ====================== Exported API =======================

% @doc Construct HTTP request record to be requested.
% @spec make_request(
%		Addr::tuple(),
%		Port::integer(),
%		Method::('head' | 'get' | 'put' | 'post' | 'trace' | 'options' | 'delete'),
%		Path::string(),
%		Version::string(),
%		Headers::[{Field::string(), Value::string()}],
%		Body::string()
%	   ) -> #http_req{}
% @end
% {{{
-spec make_request(
		Addr::tuple(),
		Port::integer(),
		Method::('head' | 'get' | 'put' | 'post' | 'trace' | 'options' | 'delete'),
		Path::string(),
		Version::string(),
		Headers::[{Field::string(), Value::string()}],
		Body::string()
	   ) -> #http_req{}.

make_request(Addr, Port, Method, Path, Version, Headers, Body) ->
	MethodStr = case Method of
		head -> "HEAD";
		get -> "GET";
		put -> "PUT";
		post -> "POST";
		trace -> "TRACE";
		options -> "OPTIONS";
		delete -> "DELETE";
		_ -> erlang:error("invalid method")
	end,
	% default HTTP request version to HTTP/1.0
	VerStr = if	length(Version) =:= 0 -> "HTTP/1.0"; true -> Version end,
	#http_req{
		addr = Addr,
		port = Port,
		method = MethodStr,
		path = Path,
		ver = VerStr,
		headers = Headers,
		body = Body
	}.
% }}}

% @doc Make a HTTP request and returns responses.
% @spec request(Req::#http_req{}) ->
%	{ok,
%		{
%			{Version::string(), Status::integer(), Reason::string()},
%			[{Field::string(), Value::string()}],
%			Body::string()
%		}
%	} | {error, Reason::term()}.
% @end
% {{{
-spec request(Req::#http_req{}) ->
	{ok,
		{
			{Version::string(), Status::integer(), Reason::string()},
			[{Field::string(), Value::string()}],
			Body::string()
		}
	} | {error, Reason::term()}.

request(Req) ->
	Addr = Req#http_req.addr,
	Port = Req#http_req.port,
	% 1. connect to target server
	case gen_tcp:connect(Addr, Port, [binary, {packet, 0}, {active, false}]) of
		{error, CReason} -> {error, CReason};
		{ok, Sock} ->
			% 2. make HTTP request
			HTTPReqLine = [
					Req#http_req.method, " ",
					Req#http_req.path, " ",
					Req#http_req.ver, "\r\n"
					],
			HTTPReqHeaders = [[F, ": ", V, "\r\n"] || {F,V} <- Req#http_req.headers],
			HTTPReq = [HTTPReqLine, HTTPReqHeaders, "\r\n", Req#http_req.body],
			Result = case gen_tcp:send(Sock, HTTPReq) of
				{error, SReason} -> {error, SReason};
				ok ->
					% 3. receive and processing HTTP response
					recv_http_resp(Sock, <<>>)
			end,
			gen_tcp:close(Sock),
			Result
	end.
% }}}

%% =================== Internal Functions ====================

% @doc Receive and parse HTTP responses.
% @end
% {{{
-spec recv_http_resp(Sock::port(), Buf::binary()) ->
	{ok,
		{
			{Version::string(), Status::integer(), Reason::string()},
			[{Field::string(), Value::string()}],
			Body::string()
		}
	} | {error, Reason::term()}.

recv_http_resp(Sock, Buf) ->
	case erlang:decode_packet(http, Buf, []) of
		{error, DReason} -> {error, DReason};
		{more, _} ->
			case gen_tcp:recv(Sock, 0) of
				{error, RReason} -> {error, RReason};
				{ok, B} ->
					NBuf = iolist_to_binary([Buf, B]),
					recv_http_resp(Sock, NBuf)
			end;
		{ok, {http_response, {Maj, Min}, Code, Reason}, Rest} ->
			% response line received
			Ver = lists:flatten([
					"HTTP/",
					integer_to_list(Maj),
					".",
					integer_to_list(Min)
					]),
			% parse response headers and body
			recv_http_header(Sock, {ok, {{Ver, Code, Reason}, [], ""}}, Rest);
		_ -> erlang:error("incorrect response")
	end.
% }}}

% @doc Receive and parse response headers and body
% @end
% {{{
-spec recv_http_header(Sock::port(), Res::tuple(), Buf::binary()) ->
	{ok,
		{
			{Version::string(), Status::integer(), Reason::string()},
			[{Field::string(), Value::string()}],
			Body::string()
		}
	} | {error, Reason::term()}.

recv_http_header(Sock, {ok, {{Ver, Code, Reason}, Headers, Body}} = Res, Buf) ->
	case erlang:decode_packet(httph, Buf, []) of
		{error, DReason} -> {error, DReason};
		{more, _} ->
			case gen_tcp:recv(Sock, 0) of
				{error, RReason} -> {error, RReason};
				{ok, B} ->
					NBuf = iolist_to_binary([Buf, B]),
					recv_http_header(Sock, Res, NBuf)
			end;
		{ok, {http_header, _, FieldAtom, _, Value}, Rest} when is_atom(FieldAtom) ->
			% process common response headers
			Field = atom_to_list(FieldAtom),
			NHeaders = [{Field, Value} | Headers],
			recv_http_header(Sock, {ok, {{Ver, Code, Reason}, NHeaders, Body}}, Rest);
		{ok, {http_header, _, Field, _, Value}, Rest} when is_list(Field) ->
			% process customized response headers
			NHeaders = [{Field, Value} | Headers],
			recv_http_header(Sock, {ok, {{Ver, Code, Reason}, NHeaders, Body}}, Rest);
		{ok, http_eoh, Rest} ->
			% HTTP header part end, the rest is body data
			ContentLength = case lists:keyfind("Content-Length", 1, Headers) of
				{_, Val} -> list_to_integer(Val);
				false -> 0
			end,
			{ok,
				{
					{Ver, Code, Reason},
					lists:reverse(Headers),
					if	ContentLength > 0 ->
							binary_to_list(recv_http_body(Sock, ContentLength - byte_size(Rest), [Rest]));
						true ->
							<<>>
					end
				}
			}
	end.
% }}}

% @doc Receive response body.
% @end
% {{{
-spec recv_http_body(Sock::port(), Len::integer(), LB::list(binary())) -> binary().

recv_http_body(_, N, LB) when N =< 0 ->
	iolist_to_binary(lists:reverse(LB));

recv_http_body(Sock, RLen, LB) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, B} -> recv_http_body(Sock, RLen - byte_size(B), [B | LB]);
		_ -> recv_http_body(Sock, 0, LB)
	end.
% }}}

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4

