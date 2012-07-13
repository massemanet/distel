-module(erlang_compile_server).

-include_lib("eunit/include/eunit.hrl").

-export([get_warnings/2
	 , get_warnings/3
	 , get_warnings_from_string/2
	 , check_eunit/2
	 , eunit_loop/1
	 , xref/1
	 , xref_start/1
	 , check_dialyzer/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang Compile Server %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_warnings(Path, Includes) ->
    get_warnings(Path, [], Includes).
get_warnings(Path, Outdir, Includes) ->
    Module = filename:absname(Path),
    %% todo; this list with includes
    Incs = [{i, I} || I <- Includes],

    case compile:file(Module, [Outdir, Incs, binary, verbose, return]) of
	{ok, _Mod, _Binary, []} ->
	    {ok};
	{ok, _Modulename, _Binary, Warnings} ->
	    {e, create_list(Warnings, warning)};
	{error, Errors, Warnings} ->
	    {e, lists:keymerge(1, create_list(Errors, error),
			   create_list(Warnings, warning))}
    end.
    

%% Does not work.
get_warnings_from_string(Textstring, Includes) ->
%%    {ok, Tokens, _} = erl_scan:string(Textstring),
%%    {ok, Parse} = erl_parse:parse_form(Tokens),
%%    case compile:forms(Parse, [Includes, binary, verbose, return]) of
%%	{ok, _Mod, _Binary, []} ->
%%	    {ok};
%%
%%	{ok, _Modulename, _Binary, Warnings} ->
%%	    {w, create_list(Warnings, warning)};
%%
%%	{error, Errors, Warnings} ->
%%	    {e, lists:keymerge(1, create_list(Errors, error), create_list(Warnings, warning))};
%%
%%	E ->
%%	    {ok, distel:fmt("Something happend: ~p.", [E])}
%%    end.	
    Tmpfile = "Tmp901835",
    case file:write_file(Tmpfile, Textstring) of
	ok ->
	    Tested = get_warnings(Tmpfile, [], Includes),
	    file:delete(Tmpfile), %% maybe warning?
	    Tested;
	{error, R} ->
	    distel:fmt("Couldn't write to temporary file ~p, because ~p.", [Tmpfile, R])
    end.

create_list(ErrorList, Info) ->
    [{Line, Info, Descr} ||
	{Line, _Mod, Descr} <- lists:keysort(1,
					     lists:flatten([[Es || Es <- Errinfo] ||
							       {_File, Errinfo} <- ErrorList]))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%       EUNIT       %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	{error, {_What, {Why, How}, Where}} -> I ! {e, {get_line(How), Why, Where}}
    end;
get_errors_and_send_back([_L|Ls], I) ->
    get_errors_and_send_back(Ls, I).

%% get the line for the error
get_line([]) ->
    "unknown"; %% maybe 0 or 1?
get_line([{line, L}|_]) ->
    L;
get_line([_L|Ls]) ->
    get_line(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        XREF       %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

xref_start(Path) ->
    case whereis(ecs) of
	undefined -> xref:start(ecs),
		     xref:add_directory(ecs, Path, [{recurse, true}]);
	_ -> case xref:stop(ecs) of
		 ok -> xref_start(ecs);
		 _ -> fail
	     end
    end.

%% seems to only work when recompiled/loaded
xref(Module) ->
    Mod = list_to_atom(filename:basename(filename:rootname(Module))),
    Path = filename:dirname(Module)++"/../ebin/",
    
    %% spawn new to try to capture the unwanted output, doesnt work btw
    spawn(?MODULE, xref_start, [Path]),
   
    case whereis(ecs) of
	undefined -> {error, "Couldn't find XREF server"};
	_ ->
	    try xref:analyze(ecs, exports_not_used) of
		{ok, []} -> {ok};
		{ok, MFA} -> case [form_mfa(X) || {M,_,_} = X <- MFA, M == Mod] of
				 [] -> {ok};
				 F -> {w, F}
			     end;
		R -> {error, R}
	    catch
		R -> {error, R}
	    after
		xref_stop()
	    end
    end.

xref_stop() ->
    case whereis(ecs) of
	undefined -> ok;
	_ -> xref:stop(ecs)
    end.

form_mfa({_M, F, A}) ->
    lists:concat([F, "/", A]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     DIALYZER      %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_dialyzer(Path) ->
    DiaOpts = [{from, src_code}, {files,[Path]}, {get_warnings, true},
	       {warnings, [no_return, no_unused, no_improper_lists,
			   no_fun_app, no_match, no_opaque, no_fail_call,
			   error_handling, race_conditions, behaviours,
			   unmatched_returns, overspecs, underspecs, specdiffs]}],

    Ret = try dialyzer:run(DiaOpts) of
	      [] -> [];
	      Warnings when is_list(Warnings) -> {w, [{L, warning, Msg} || {_, {_, L}, Msg} <- Warnings]};
	      E -> E
	  catch
	      E -> E
	  end,
    case Ret of
	[] -> {ok};
	{w, _} = R -> R;
	C -> {error, C}
    end.
