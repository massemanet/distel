%%%-------------------------------------------------------------
%%% File    : erlang_compile_server.erl
%%% Author  : Sebastian Weddmark Olsson
%%%           github.com/sebastiw
%%% Purpose : Used with distel as a backend for erlang-compile-server
%%% 
%%% Created : June 2012 as an internship at Klarna AB
%%% Comment : Please let me know if you find any bugs or you
%%%           want some feature or something
%%%-------------------------------------------------------------
-module(erlang_compile_server).

-include_lib("eunit/include/eunit.hrl").

-export([get_warnings/3,
	 get_warnings_from_string/3,
	 check_eunit/2,
	 eunit_loop/2,
	 xref/1,
	 xref_start/1,
	 check_dialyzer/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlang Compile Server %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% get_warnings/3 takes a module, its includes and some compile options and
%%% returns the compile warnings.
get_warnings(Path, Includes, Options) ->
    Module = filename:absname(Path),
    Incs = [{i, I} || I <- Includes, I =/= []],
    case compile:file(Module, Incs++[binary, verbose, return, debug_info]++Options) of
	{ok, _Mod, _Binary, []} ->
	    {ok};
	{ok, _Modulename, _Binary, Warnings} ->
	    {e, create_list(Warnings, warning)};
	{error, Errors, Warnings} ->
	    {e, lists:keymerge(1, create_list(Errors, error),
			   create_list(Warnings, warning))}
    end.

%% get_warnings_from_string/3 uses a temporary file to store the buffer-string in
get_warnings_from_string(Textstring, Includes, Options) ->
    Tmpfile = "Tmp901835.erl",
    case file:write_file(Tmpfile, Textstring) of
	ok ->
	    Tested = get_warnings(Tmpfile, Includes, Options),
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

%%% check_eunit/2 takes a path and a caller-pid. 
%%% Then it returns the eunit results to that caller.
check_eunit(Path, Caller) ->
    Mod = list_to_atom(filename:rootname(Path)),
    Pid = spawn(?MODULE, eunit_loop, [Mod, Caller]),
    eunit:test(Mod, [{report, {ecs_listener, [{pid, Pid}, {no_tty, true}]}}]).

eunit_loop(Mod, Caller) ->
    receive
	R ->
	    get_errors_and_send_back(Mod, R, Caller),
	    eunit_loop(Mod, Caller)
    after 1500 ->
	    Caller ! {klar}
    end.

get_errors_and_send_back(_, [], _) ->
    void;
get_errors_and_send_back(Mod, [{status, S}|Ls], I) ->
    case S of
	ok -> I ! {ok, get_ok_test(Ls)};
	{error, {_What, {Why, How}, [{M, F, A}|R]}} ->
		case M == Mod of
		    true -> I ! {e, {get_line(How), Why, [{M,F,A}|R]}};
		    false -> I ! {e, {0, Why, {get_line(How), [{M,F,A}|R]}}}
		end
    end;
get_errors_and_send_back(Mod, [_L|Ls], I) ->
    get_errors_and_send_back(Mod, Ls, I).

%% get the line for the error
get_line([]) ->
    0;
get_line([{line, L}|_]) ->
    L;
get_line([_L|Ls]) ->
    get_line(Ls).

get_ok_test([]) ->
    void;
get_ok_test([{source, MFA}|Ls]) ->
    {get_line(Ls), MFA};
get_ok_test([_L|Ls]) ->
    get_ok_test(Ls).


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
%%% xref/1 takes a filepath and asks xref if there is any
%%% unused exported function in that module
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

%%% check_dialyzer/1 takes a path and runs dialyzer tests on the files of that path
check_dialyzer(Path) ->
    DiaOpts = [{from, src_code}, {files,[Path]}, {get_warnings, true},
	       {warnings, [no_return, no_unused, no_improper_lists,
			   no_fun_app, no_match, no_opaque, no_fail_call,
			   error_handling, race_conditions, %%behaviours,
			   unmatched_returns, overspecs, underspecs, specdiffs]}],

    Ret = try dialyzer:run(DiaOpts) of
	      [] -> [];
	      Warnings when is_list(Warnings) ->
		  {w, [{L, warning, format_msg(Msg)} || {_, {_, L}, Msg} <- Warnings]};
	      E -> E
	  catch
	      E -> E
	  end,
    case Ret of
	[] -> {ok};
	{w, _} = R -> R;
	C -> {error, C}
    end.


%%% format_msg/1 takes a tuple of a type and a message and makes the message binary.
format_msg({Type, Msg}) ->
  {Header, Messages} = lists:split(3, Msg),
  FormattedMessages = lists:map(fun erlang:iolist_to_binary/1, Messages),
  {Type, lists:append([Header, FormattedMessages])}.
