{application,
	zfor, 	% Application name, the resource file name must be of the form <Application name>.app
	[
		{description, "Z Fail-over Resolver"}, 	% Application description, used by systools
		{vsn, "1.0"}, 	% Application version string, used by systools
		{modules,
			[
				zfor_app,
				zfor_sup,
				zfor_main,
				zfor_server,
				zfor_config,
				zfor_caretaker,
				zfor_util
			]
		}, 		% All modules *introduced* by this application
		{registered,
			[
				zfor_main,
				zfor_sup
			]
		}, 	% All names of registered processes in the application,
			% systools uses this list to detect name clashes between 
			% applications
		{applications, [kernel, stdlib, crypto, inets]}, 	% All applications which must be started before this
															% application is started. systools uses this list.
		{env,[{conf_path, "/home/z/conf/zfor"}]}, 	% Application configuration parameters, [{Par,Val}]
											% Can be override in system configuration file / cmdline
		{mod,{zfor_app,[]}} 	% The startup module and start arguments
	]
}.
% vim:ft=erlang ts=4 sw=4

