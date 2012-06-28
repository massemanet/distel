-module(erlang_compile_server).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang Compile Server %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_warnings(Path) ->
    get_warnings(Path, [], []).
get_warnings(Path, Includes) ->
    get_warnings(Path, [], Includes).
get_warnings(Path, Outdir, Includes) ->
    Module = filename:absname(Path),
    case compile:file(Module, [Outdir, Includes, binary, verbose, return]) of
	{ok, _Mod, _Binary, []} ->
	    {ok};

	{ok, _Modulename, _Binary, Warnings} ->
	    {w, create_list(Warnings, warning)};

	{error, Errors, Warnings} ->
	    {e, lists:keymerge(1, create_list(Errors, error), create_list(Warnings, warning))};

	E ->
	    {ok, distel:fmt("Something happend: ~p.", [E])}
    end.

create_list(ErrorList, Info) ->
    [{Line, Info, Descr} ||
	{Line, _Mod, Descr} <- lists:keysort(1,
					     lists:flatten([[Es || Es <- Errinfo] ||
							       {_File, Errinfo} <- ErrorList]))].

get_warnings_from_buffer(Textstring) ->
    get_warnings_from_buffer(Textstring, []).
get_warnings_from_buffer(Textstring, Includes) ->
    Tmpfile = "Tmp901835",
    case file:write_file(Tmpfile, Textstring) of
	ok ->
	    Tested = get_warnings(Tmpfile, Includes),
	    file:delete(Tmpfile), %% maybe warning?
	    Tested;
	{error, R} ->
	    distel:fmt("Couldn't write to temporary file ~p, because ~p.", [Tmpfile, R])
    end.
