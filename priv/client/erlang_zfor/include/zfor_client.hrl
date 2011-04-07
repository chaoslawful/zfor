-define(DEFAULT_ZFOR_SERVER, "localhost").
-define(DEFAULT_ZFOR_PORT, 1117).
-define(DEFAULT_ZFOR_TIMEOUT, 500).

-define(ZFOR_REQ_DNS, 0).
-define(ZFOR_REQ_GET_VCONF, 1).

% @type zfor_client_ctx() Context parameters needed by zfor client.
-record(zfor_client_ctx, {
		server = ?DEFAULT_ZFOR_SERVER :: string()
		, port = ?DEFAULT_ZFOR_PORT :: integer()
		, timeout = ?DEFAULT_ZFOR_TIMEOUT :: integer()
	}).
-type zfor_client_ctx() :: #zfor_client_ctx{}.

