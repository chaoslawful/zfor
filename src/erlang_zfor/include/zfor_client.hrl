-define(DEFAULT_ZFOR_SERVER, "localhost").
-define(DEFAULT_ZFOR_PORT, 1117).
-define(DEFAULT_TIMEOUT, 500).

-define(REQ_DNS, 0).

-define(ERR, error_logger:error_msg).
-define(WARN, error_logger:warning_msg).
-define(INFO, error_logger:info_msg).

% @type zfor_client_ctx() Context parameters needed by zfor client.
-record(zfor_client_ctx, {
		server = ?DEFAULT_ZFOR_SERVER :: string()
		, port = ?DEFAULT_ZFOR_PORT :: integer()
		, timeout = ?DEFAULT_TIMEOUT :: integer()
	}).
-type zfor_client_ctx() :: #zfor_client_ctx{}.

