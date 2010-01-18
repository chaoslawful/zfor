-record(http_req, {
		addr::tuple(),
		port::integer(),
		method::string(),
		path::string(),
		ver::string(),
		headers::[{string(),string()}],
		body::string()
		}).

% vim600: noet ft=erlang ts=4 sw=4 fdm=marker
% vim<600: noet ft=erlang ts=4 sw=4
