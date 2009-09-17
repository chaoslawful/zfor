-module(gen_rel).
-export([main/0]).

-define(REL_FILE, "zfor.rel").
-define(ZFOR_VER, "1.0").

main() ->
	{ok, Fd} = file:open(?REL_FILE, [write]),
	io:format(Fd, "% zfor release file, for generating boot scripts\n", []),
	io:format(Fd, "{release,\n", []),
	io:format(Fd, "    {\"zfor\", \"~s\"},\n", [?ZFOR_VER]),
	io:format(Fd, "    {erts, \"~s\"},\n", [erlang:system_info('version')]),
	io:format(Fd, "    [\n", []),
	output_lib_version(Fd, [kernel, stdlib, sasl, crypto, inets, {zfor, ?ZFOR_VER}], "        "),
	io:format(Fd, "    ]\n", []),
	io:format(Fd, "}.\n", []),
	ok = file:close(Fd),
	io:format("~s generated!\n", [?REL_FILE]).


output_lib_version(_, [], _) -> 'ok';
output_lib_version(Fd, [Lib], PrefixStr) when is_atom(Lib) ->
	VerStr = lib_verstr(Lib),
	io:format(Fd, "~s{~s, \"~s\"}\n", [PrefixStr, Lib, VerStr]);
output_lib_version(Fd, [{Lib, VerStr}], PrefixStr) when is_atom(Lib), is_list(VerStr) ->
	io:format(Fd, "~s{~s, \"~s\"}\n", [PrefixStr, Lib, VerStr]);
output_lib_version(Fd, [{Lib, VerStr} | Rest], PrefixStr) when is_atom(Lib), is_list(VerStr) ->
	io:format(Fd, "~s{~s, \"~s\"},\n", [PrefixStr, Lib, VerStr]),
	output_lib_version(Fd, Rest, PrefixStr);
output_lib_version(Fd, [Lib | Rest], PrefixStr) when is_atom(Lib) ->
	VerStr = lib_verstr(Lib),
	io:format(Fd, "~s{~s, \"~s\"},\n", [PrefixStr, Lib, VerStr]),
	output_lib_version(Fd, Rest, PrefixStr).

lib_verstr(Lib) when is_atom(Lib) ->
	LibDir = code:lib_dir(Lib),
	string:substr(LibDir, string:rchr(LibDir, $-) + 1).

