-module(erlang_compile_server).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang Compile Server %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_to_compile(Path) ->
    Module = filename:absname(Path),
    case compile:file(Module, [binary, verbose, return]) of
	{ok, _Mod, _Binary, []} ->
	    {ok};
	{ok, _Modulename, _Binary, Warnings} ->
	    {w, Warnings};
	{error, Errors, Warnings} ->
	    {e, Errors, Warnings};
	_ ->
	    {ok, distel:fmt("Nothing to do.", "")}
    end.
