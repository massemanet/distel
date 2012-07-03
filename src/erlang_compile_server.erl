-module(erlang_compile_server).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang Compile Server %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

check_eunit(Path, Caller) ->
    Mod = list_to_atom(filename:rootname(Path)),
    Pid = spawn(?MODULE, eunit_loop, [Caller]),
    eunit:test(Mod, [{report, {ecs_listener, [{pid, Pid}, {no_tty, true}]}}]).

eunit_loop(Caller) ->
    receive
	R ->
	    get_errors_and_send_back(R, Caller),
	    eunit_loop(Caller)
    after 1500 ->
	    Caller ! {klar}
    end.

get_errors_and_send_back([], _) ->
    void;
get_errors_and_send_back([{status, S}|_Ls], I) ->
    case S of
	ok -> void;
	{error, {_What, {Why, How}, Where}} -> I ! {e, {Why, get_line(How), Where}}
    end;
get_errors_and_send_back([_L|Ls], I) ->
    get_errors_and_send_back(Ls, I).

%% get the line for the error
get_line([]) ->
    "unknown";
get_line([{line, L}|_]) ->
    L;
get_line([_L|Ls]) ->
    get_line(Ls).

create_list(ErrorList, Info) ->
    [{Line, Info, Descr} ||
	{Line, _Mod, Descr} <- lists:keysort(1,
					     lists:flatten([[Es || Es <- Errinfo] ||
							       {_File, Errinfo} <- ErrorList]))].

%% Does not work.
get_warnings_from_string(Textstring, Includes) ->
    get_warnings_from_string(Textstring, [], Includes).
get_warnings_from_string(Textstring, Outdir, Includes) ->
%    {ok, Tokens, _} = erl_scan:string(Textstring),
%    {ok, Parse} = erl_parse:parse_form(Tokens),
%    case compile:forms(Parse, [Outdir, Includes, binary, verbose, return]) of
%	{ok, _Mod, _Binary, []} ->
%	    {ok};
%
%	{ok, _Modulename, _Binary, Warnings} ->
%	    {w, create_list(Warnings, warning)};
%
%	{error, Errors, Warnings} ->
%	    {e, lists:keymerge(1, create_list(Errors, error), create_list(Warnings, warning))};
%
%	E ->
%	    {ok, distel:fmt("Something happend: ~p.", [E])}
%    end.	

    Tmpfile = "Tmp901835",
    case file:write_file(Tmpfile, Textstring) of
	ok ->
	    Tested = get_warnings(Tmpfile, Outdir, Includes),
	    file:delete(Tmpfile), %% maybe warning?
	    Tested;
	{error, R} ->
	    distel:fmt("Couldn't write to temporary file ~p, because ~p.", [Tmpfile, R])
    end.
