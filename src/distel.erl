%%%-------------------------------------------------------------------
%%% File    : distel.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Helper functions to be called from Emacs.
%%%
%%% Created : 18 Mar 2002 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(distel).

-author('luke@bluetail.com').

-include_lib("kernel/include/file.hrl").

-import(lists, [flatten/1, member/2, sort/1, map/2, foldl/3, foreach/2]).
-import(filename, [dirname/1,join/1,basename/2]).

-export([rpc_entry/3, eval_expression/1, find_source/1,
         process_list/0, process_summary/1,
         process_summary_and_trace/2, fprof/3, fprof_analyse/1,
         debug_toggle/2, debug_subscribe/1, debug_add/1,
         break_toggle/2, break_delete/2, break_add/2, break_restore/1,
         modules/1, functions/2,
         free_vars/1, free_vars/2,
         apropos/1, apropos/2, describe/3, describe/4]).

-export([reload_module/2,reload_modules/0]).

-export([gl_proxy/1, tracer_init/2, null_gl/0]).

-compile(export_all).

to_bin(X) -> list_to_binary(to_list(X)).
to_atom(X) -> list_to_atom(to_list(X)).
     
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_integer(X)-> integer_to_list(X);
to_list(X) when is_float(X)  -> float_to_list(X);
to_list(X) when is_atom(X)   -> atom_to_list(X);
to_list(X) when is_list(X)   -> X.		%Assumed to be a string

%% ----------------------------------------------------------------------
%% RPC entry point, adapting the group_leader protocol.

rpc_entry(M, F, A) ->
    GL = group_leader(),
    Name = gl_name(GL),
    case whereis(Name) of
        undefined ->
            Pid = spawn(?MODULE, gl_proxy, [GL]),
            register(Name, Pid),
            group_leader(Pid, self());
        Pid ->
            group_leader(Pid, self())
    end,
    apply(M,F,A).

gl_name(Pid) ->
    to_atom(flatten(io_lib:format("distel_gl_for_~p", [Pid]))).

gl_proxy(GL) ->
    receive
        {io_request, From, ReplyAs, {put_chars, C}} ->
            GL ! {put_chars, C},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, M, F, A}} ->
            GL ! {put_chars, flatten(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {get_until, _, _, _}} ->
            %% Input not supported, yet
            From ! {io_reply, ReplyAs, eof}
    end,
    gl_proxy(GL).

%% ----------------------------------------------------------------------
%%% reload all modules that are out of date
%%% compare the compile time of the loaded beam and the beam on disk
reload_modules() ->
    T = fun(L) -> [X || X <- L, element(1,X)==time] end,
    Tm = fun(M) -> T(M:module_info(compile)) end,
    Tf = fun(F) -> {ok,{_,[{_,I}]}}=beam_lib:chunks(F,[compile_info]),T(I) end,
    Load = fun(M) -> c:l(M),M end,
    
    [Load(M) || {M,F} <- code:all_loaded(), is_file(F), Tm(M)<Tf(F)].

is_file(F) -> ok == element(1,file:read_file_info(F)).
    
%% ----------------------------------------------------------------------
%% if c:l(Mod) doesn't work, we look for the beam file in 
%% srcdir and srcdir/../ebin; add the first one that works to path and 
%% try c:l again.
%% srcdir is dirname(EmacsBuffer); we check that the buffer contains Mod.erl
%% emacs will set File = "" to indicate that we don't want to mess with path
reload_module(Mod, File) ->
    case c:l(to_atom(Mod)) of
	{error,R} ->
	    case Mod == basename(File,".erl") of
		true ->
		    Beams = [join([dirname(File),Mod]),
			     join([dirname(dirname(File)),ebin,Mod])],
			case lists:filter(fun is_beam/1, Beams) of
			    [] -> {error, R};
			    [Beam|_] -> 
				code:add_patha(dirname(Beam)),
				c:l(to_atom(Mod))
			end;
		false ->
		    {error,R}
	    end;
	R -> R
    end.

is_beam(Filename) ->
    case file:read_file_info(Filename++".beam") of
	{ok,_} -> true;
	{error,_} -> false
    end.
	    
%% ----------------------------------------------------------------------

eval_expression(S) ->
    case parse_expr(S) of
        {ok, Parse} ->
            try_evaluation(Parse);
        {error, {_, erl_parse, Err}} ->
            {error, Err}
    end.

try_evaluation(Parse) ->
    case catch erl_eval:exprs(Parse, []) of
        {value, V, _} ->
            {ok, flatten(io_lib:format("~p", [V]))};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

parse_expr(S) ->
    {ok, Scan, _} = erl_scan:string(S),
    erl_parse:parse_exprs(Scan).

find_source(Mod) ->
    case beamfile(Mod) of
	{ok, BeamFName} ->
	    case guess_source_file(Mod, BeamFName) of
		{true, SrcFName} ->
		    {ok, SrcFName};
		false ->
		    {error, fmt("Can't guess matching source file from ~p",
				[BeamFName])}
	    end;
	error ->
            {error, fmt("Can't find module '~p' on ~p", [Mod, node()])}
    end.

%% Ret: {true, AbsName} | false
guess_source_file(Mod, BeamFName) ->
    Erl = to_list(Mod) ++ ".erl",
    Dir = dirname(BeamFName),
    TryL = src_from_beam(BeamFName) ++
	[Dir ++ "/" ++ Erl,
	 join([Dir ++ "/../src/", Erl]),
	 join([Dir ++ "/../esrc/", Erl]),
	 join([Dir ++ "/../erl/", Erl])],
    try_srcs(TryL).

try_srcs([H | T]) ->
    case file:read_file_info(H) of
	{ok, #file_info{type = regular}} ->
	    {true, H};
	_ ->
	    try_srcs(T)
    end;
try_srcs([]) ->
    false.

src_from_beam(BeamFile) ->
    case beam_lib:chunks(BeamFile, ["CInf"]) of
	{ok, {_, [{"CInf", Bin}]}} ->
	    case catch binary_to_term(Bin) of
		L when list(L) ->
		    case lists:keysearch(source, 1, L) of
			{value, {_, Source}} ->
			    [Source];
			_ ->
			    []
		    end;
		_ ->
		    []
	    end;
	_ ->
	    []
    end.

%% ----------------------------------------------------------------------
%% Summarise all processes in the system.
%%
%% Returns: {Heading, [{pid(), Row}]}
%% Heading = Row = binary()
process_list() ->
    Heading = iformat("PID/Name", "Initial Call", "Reds", "Msgs"),
    {Heading, [{Pid, iformat_pid(Pid)} || Pid <- processes()]}.

iformat_pid(Pid) ->
    iformat(name(Pid), initial_call(Pid), reductions(Pid), messages(Pid)).

name(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Regname} ->
            to_list(Regname);
        _ ->
            io_lib:format("~p", [Pid])
    end.

initial_call(Pid) ->
    {initial_call, {M, F, A}} = process_info(Pid, initial_call),
    io_lib:format("~s:~s/~p", [M, F, A]).

reductions(Pid) ->
    {reductions, NrReds} = process_info(Pid, reductions),
    to_list(NrReds).

messages(Pid) ->
    {messages, MsgList} = process_info(Pid, messages),
    to_list(length(MsgList)).

iformat(A1, A2, A3, A4) ->
    to_bin(io_lib:format("~-21s ~-33s ~12s ~8s~n",
                                 [A1,A2,A3,A4])).

%% ----------------------------------------------------------------------
%% Individual process summary and tracing.

%% Returns: {ok, String} | {error, Rsn}
process_info_item(Pid, Item) ->
    case process_info(Pid, Item) of
        {backtrace, Bin} ->
            {ok, Bin};
        {Item, Term} ->
            {ok, fmt("~p~n", [Term])};
        undefined ->
            case is_process_alive(Pid) of
                true ->
                    {ok, <<"undefined">>};
                false ->
                    {ok, fmt("dead process: ~p", [Pid])}
            end
    end.

%% Returns: Summary : binary()
process_summary(Pid) ->
    Text = [io_lib:format("~-20w: ~w~n", [Key, Value])
            || {Key, Value} <- [{pid, Pid} | process_info(Pid)]],
    to_bin(Text).

%% Returns: Summary : binary()
%%
%% Tracer is sent messages of:
%%    {trace_msg, binary()}
process_summary_and_trace(Tracer, Pid) ->
    case is_process_alive(Pid) of
        true ->
            spawn_tracer(Tracer, Pid),
            process_summary(Pid);
        false ->
            {error, fmt("dead process: ~p", [Pid])}
    end.

spawn_tracer(Tracer, Tracee) ->
    spawn(?MODULE, tracer_init, [Tracer, Tracee]).

tracer_init(Tracer, Tracee) ->
    link(Tracer),
    erlang:trace(Tracee, true, trace_flags()),
    tracer_loop(Tracer, Tracee).

trace_flags() ->
    [send, 'receive', running, procs, garbage_collection, call, return_to].

tracer_loop(Tracer, Tracee) ->
    receive
        Trace when tuple(Trace),
                   element(1, Trace) == trace,
                   element(2, Trace) == Tracee ->
            Msg = tracer_format(Trace),
            Tracer ! {trace_msg, to_bin(Msg)}
    end,
    tracer_loop(Tracer, Tracee).

tracer_format(Msg) ->
    %% Let's steal some code reuse..
    pman_buf_utils:textformat(Msg).

%% ----------------------------------------------------------------------
%% Profiling
%% fprof_expr(E) -> {ok, Preamble, Header, Entry}
%% Preamble = binary()
%% Entry = {Tag, MFA, Text, Callers, Callees, Beamfile}
%% Callers = Callees = [Tag]
%% MFA = [Module, Function, Arity] | undefined
%%
%% Entry example,
%%   {'foo:bar/2', "foo:bar/2  10 100 200", ['baz:beer/2'], [], "/foo.beam"}

fprof(Expr) ->
    case parse_expr(Expr) of
        {ok, Parse} ->
            fprof_fun(fun() -> erl_eval:exprs(Parse, []) end);
        {error, Rsn} ->
            {error, Rsn}
    end.

fprof(M,F,A) ->
    fprof_fun(fun() -> apply(M,F,A) end).

fprof_fun(F) ->
    GL = spawn_link(fun null_gl/0),
    group_leader(GL, self()),
    fprof:apply(F, []),
    fprof:profile(),
    fprof:analyse({dest, "/tmp/fprof.analysis"}),
    GL ! die,
    fprof_analyse("/tmp/fprof.analysis").

fprof_analyse(Filename) ->
    {ok, Asys} = file:consult(Filename),
    [_Opts, [Totals], _Proc | Fns] = Asys,
    {ok,
     fprof_preamble(Totals),
     fprof_header(),
     [fprof_entry(Entry) || Entry <- Fns]}.
    
fprof_preamble({totals, Cnt, Acc, Own}) ->
    fmt("Totals: ~p calls, ~.3f real, ~.3f CPU\n\n", [Cnt, Acc, Own]).

fprof_header() ->
    fmt("~sCalls\tACC\tOwn\n", [pad(50, "Function")]).

fprof_entry([{ProcName, _Cnt, _Acc, Own} | Info]) ->
    %% {process, Name, [Infos]}
    {process, fmt("Process ~s: ~p%", [ProcName, Own]),
     map(fun fprof_process_info/1, Info)};
fprof_entry(F) ->
    {Up, This, Down} = F,
    {Name, _, _, _} = This,
    {tracepoint,
     fprof_tag(Name),
     fprof_mfa(Name),
     fprof_text(This),
     fprof_tags(Up),
     fprof_tags(Down),
     fprof_beamfile(Name)}.

fprof_process_info({spawned_by, Who}) ->
    fmt("  ~s: ~s", [pad(16, "spawned_by"), Who]);
fprof_process_info({spawned_as, What}) ->
    fmt("  ~s: ~s", [pad(16, "spawned as"),
                   fprof_tag_name(What)]);
fprof_process_info({initial_calls, Calls}) ->
    fmt("  ~s: ~p~n", [pad(16, "initial calls"), Calls]);
fprof_process_info(Info) ->
    fmt("  ???: ~p~n", [Info]).

fprof_tag({M,F,A}) when integer(A) ->
    to_atom(flatten(io_lib:format("~p:~p/~p", [M,F,A])));
fprof_tag({M,F,A}) when list(A) ->
    fprof_tag({M,F,length(A)});
fprof_tag(Name) when  atom(Name) ->
    Name.

fprof_mfa({M,F,A}) -> [M,F,A];
fprof_mfa(_)       -> undefined.

fprof_tag_name(X) -> flatten(io_lib:format("~s", [fprof_tag(X)])).

fprof_text({Name, Cnt, Acc, Own}) ->
    fmt("~s~p\t~.3f\t~.3f\n",
        [pad(50, fprof_tag_name(Name)), Cnt, Acc, Own]).

fprof_tags(C) -> [fprof_tag(Name) || {Name,_,_,_} <- C].

fprof_beamfile({M,_,_}) ->
    case code:which(M) of
        Fname when list(Fname) ->
            to_bin(Fname);
        _ ->
            undefined
    end;
fprof_beamfile(_)                  -> undefined.

fmt(X, A) -> to_bin(io_lib:format(X, A)).

pad(X, A) when atom(A) ->
    pad(X, to_list(A));
pad(X, S) when length(S) < X ->
    S ++ lists:duplicate(X - length(S), $ );
pad(_X, S) ->
    S.

null_gl() ->
    receive
        {io_request, From, ReplyAs, _} ->
            From ! {io_reply, ReplyAs, ok},
            null_gl();
        die ->
            ok
    end.

%% ----------------------------------------------------------------------
%% Debugging
%% ----------------------------------------------------------------------
debug_toggle(Mod, Filename) ->
    case member(Mod, int:interpreted()) of
        true ->
            int:n(Mod),
            uninterpreted;
        false ->
            code:ensure_loaded(Mod),
            case int:i(Filename) of
                {module, Mod} ->
                    interpreted;
                error ->
                    error
            end
    end.

debug_add(Modules) ->
    foreach(fun([_Mod, FileName]) ->
		    %% FIXME: want to reliably detect whether
		    %% the module is interpreted, but
		    %% 'int:interpreted()' can give the wrong
		    %% answer if code is reloaded behind its
		    %% back.. -luke
		    int:i(FileName)
	    end, Modules),
    ok.

break_toggle(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line),
            disabled;
        false ->
            ok = int:break(Mod, Line),
            enabled
    end.

break_delete(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line);
        false ->
            ok
    end.

break_add(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok;
        false ->
            ok = int:break(Mod, Line)
    end.

%% L = [[Module, [Line]]]
break_restore(L) ->
    foreach(
      fun([Mod, Lines]) ->
              foreach(fun(Line) -> int:break(Mod, Line) end, Lines)
      end, L),
    ok.


fname(Mod) ->
    filename:rootname(code:which(Mod), "beam") ++ "erl".

%% Returns: {InterpretedMods, Breakpoints, [{Pid, Text}]}
%%          InterpretedMods = [[Mod, File]]
%%          Breakpoints     = [{Mod, Line}]
debug_subscribe(Pid) ->
    %% NB: doing this before subscription to ensure that the debugger
    %% server is started (int:subscribe doesn't do this, probably a
    %% bug).
    Interpreted = lists:map(fun(Mod) -> [Mod, fname(Mod)] end,
                            int:interpreted()),
    spawn_link(?MODULE, debug_subscriber_init, [self(), Pid]),
    receive ready -> ok end,
    int:clear(),
    {Interpreted,
     [Break || {Break, _Info} <- int:all_breaks()],
     [{Proc,
       fmt("~p:~p/~p", [M,F,length(A)]),
       fmt("~w", [Status]),
       fmt("~w", [Info])}
      || {Proc, {M,F,A}, Status, Info} <- int:snapshot()]}.

debug_subscriber_init(Parent, Pid) ->
    link(Pid),
    int:subscribe(),
    Parent ! ready,
    debug_subscriber(Pid).

debug_subscriber(Pid) ->
    receive
	{int, {new_status, P, Status, Info}} ->
	    Pid ! {int, {new_status, P, Status, fmt("~w",[Info])}};
	{int, {new_process, {P, {M,F,A}, Status, Info}}} ->
	    Pid ! {int, {new_process,
			 [P,
			  fmt("~p:~p/~p", [M,F,length(A)]),
			  Status,
			  fmt("~w", [Info])]}};
	{int, {interpret, Mod}} ->
	    Pid ! {int, {interpret, Mod, fname(Mod)}};
	Msg ->
	    Pid ! Msg
    end,
    debug_subscriber(Pid).

debug_format(Pid, {M,F,A}, Status, Info) ->
    debug_format_row(io_lib:format("~w", [Pid]),
                     io_lib:format("~p:~p/~p", [M,F,length(A)]),
                     io_lib:format("~w", [Status]),
                     io_lib:format("~w", [Info])).

debug_format_row(Pid, MFA, Status, Info) ->
    fmt("~-12s ~-21s ~-9s ~-21s~n", [Pid, MFA, Status, Info]).

%% Attach the client process Emacs to the interpreted process Pid.
%%
%% spawn_link's a new process to proxy messages between Emacs and
%% Pid's meta-process.
debug_attach(Emacs, Pid) ->
    spawn_link(?MODULE, attach_init, [Emacs, Pid]).

%% State for attached process, based on `dbg_ui_trace' in the debugger.
-record(attach, {emacs,                 % pid()
                 meta,                  % pid()
                 status,                % break | running | idle | ...
                 where,                 % {Mod, Line}
                 stack                  % {CurPos, MaxPos}
                }).

attach_init(Emacs, Pid) ->
    link(Emacs),
    case int:attached(Pid) of
        {ok, Meta} ->
            attach_loop(#attach{emacs=Emacs,
				meta=Meta,
				status=idle,
				stack={undefined,undefined}});
        error ->
            exit({error, {unable_to_attach, Pid}})
    end.

attach_loop(Att = #attach{emacs=Emacs, meta=Meta}) ->
    receive 
	{Meta, {break_at, Mod, Line, Pos}} ->
	    Att1 = Att#attach{status=break,
			      where={Mod, Line},
			      stack={Pos, Pos}},
	    ?MODULE:attach_loop(attach_goto(Att1,Att1#attach.where));
	{Meta, Status} when atom(Status) ->
	    Emacs ! {status, Status},
	    ?MODULE:attach_loop(Att#attach{status=Status,where=undefined});
	{NewMeta, {exit_at,null,_R,Pos}} when is_pid(NewMeta) ->
	    %% exit, no stack info
	    Att1 = Att#attach{status=exit,
			      where=undefined,
			      meta=NewMeta,
			      stack={Pos, Pos}},
	    ?MODULE:attach_loop(Att1);
	{NewMeta, {exit_at,{Mod,Line},_R,Pos}} when is_pid(NewMeta) ->
	    %% exit on error, there is stack info
	    Att1 = Att#attach{meta = NewMeta,
			      status=break,
			      where={Mod,Line},
			      stack={Pos+1, Pos+1}},
	    ?MODULE:attach_loop(attach_goto(Att1,Att1#attach.where));
	{Meta, {attached,_Mod,_Lin,_Flag}} ->
	    %% happens first time we attach, presumably because we (or
	    %% someone else) set a break here
	    ?MODULE:attach_loop(Att);
	{Meta,{re_entry, Mod, Func}} -> 
	    Msg = "re_entry "++to_list(Mod)++to_list(Func),
	    Emacs ! {message, to_bin(Msg)},
	    ?MODULE:attach_loop(Att);
	{Meta, _X} ->
	    %%io:fwrite("distel:attach_loop OTHER meta: ~p~n", [_X]),
	    %% FIXME: there are more messages to handle, like re_entry
	    ?MODULE:attach_loop(Att);
	{emacs, meta, Cmd} when Att#attach.status == break ->
	    attach_loop(attach_meta_cmd(Cmd, Att));
	{emacs, meta, _Cmd} ->
	    Emacs ! {message, <<"Not in break">>},
	    ?MODULE:attach_loop(Att);
	_X ->
	    %%io:fwrite("distel:attach_loop OTHER: ~p~n", [_X]),
	    ?MODULE:attach_loop(Att)
    end.

attach_meta_cmd(up, Att = #attach{stack={Pos,Max}}) ->
    case int:meta(Att#attach.meta, stack_frame, {up, Pos}) of
	{NPos, {undefined, -1},[]} ->  %OTP R10
	    Att#attach.emacs ! {message, <<"uninterpreted code.">>},
	    Att#attach{stack={NPos,Max}};
	{NPos, {Mod, Line},Binds} ->  %OTP R10
	    attach_goto(Att#attach{stack={NPos,Max}},{Mod,Line},Binds);
	{NPos, Mod, Line} ->		   %OTP R9
	    attach_goto(Att#attach{stack={NPos,Max}},{Mod,Line});
	top ->
	    Att#attach.emacs ! {message, <<"already at top.">>},
	    Att
    end;
attach_meta_cmd(down, Att = #attach{stack={_Max,_Max}}) ->
    Att#attach.emacs ! {message, <<"already at bottom">>},
    Att;
attach_meta_cmd(down, Att = #attach{stack={Pos,Max}}) ->
    case int:meta(Att#attach.meta, stack_frame, {down, Pos}) of
	{NPos, {undefined, -1},[]} ->  %OTP R10
	    Att#attach.emacs ! {message, <<"uninterpreted code.">>},
	    Att#attach{stack={NPos,Max}};
	{NPos, {Mod, Line},Binds} ->  %OTP R10
	    attach_goto(Att#attach{stack={NPos,Max}},{Mod,Line},Binds);
	{NPos, Mod, Line} ->		   %OTP R9
	    attach_goto(Att#attach{stack={NPos,Max}},{Mod,Line});
	bottom ->
	    attach_goto(Att#attach{stack={Max, Max}},Att#attach.where)
    end;
attach_meta_cmd({get_binding, Var}, Att = #attach{stack={Pos,_Max}}) ->
    Bs = int:meta(Att#attach.meta, bindings, Pos),
    case lists:keysearch(Var, 1, Bs) of
	{value, Val} ->
	    Att#attach.emacs ! {show_variable, fmt("~p~n", [Val])};
	false ->
	    Att#attach.emacs ! {message, fmt("No such variable: ~p",[Var])}
    end,
    Att;
attach_meta_cmd(Cmd, Att) ->
    int:meta(Att#attach.meta, Cmd),
    Att.

attach_goto(A,ML) ->
    attach_goto(A,ML,sort(int:meta(A#attach.meta, bindings, stack_pos(A)))).
attach_goto(A = #attach{stack={Pos,Max}},{Mod,Line},Bs) ->
    Vars = [{Name, fmt("~9s = ~P~n", [Name, Val, 9])} || {Name,Val} <- Bs],
    A#attach.emacs ! {variables, Vars},
    A#attach.emacs ! {location, Mod, Line, Pos, Max},
    A.
stack_pos(#attach{stack={_X,_X}}) -> nostack;
stack_pos(#attach{stack={Pos,_Max}}) -> Pos.
    
%% ----------------------------------------------------------------------
%% Completion support
%% ----------------------------------------------------------------------

%% Returns: [ModName] of all modules starting with Prefix.
%% ModName = Prefix = string()
modules(Prefix) ->
%  FIXME: have to decide which approach is better - all loaded or all in path
%         i, of course, prefer all in path (mbj)
    Dirs = code:get_path(),
    {ok, sort(foldl(fun(Dir, Acc) -> fm_dir(Dir, Prefix, Acc) end, [], Dirs))}.

fm_dir(Dir, Prefix, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    Mods = [basename(F, ".beam") || F <- Files,
					    lists:prefix(Prefix, F),
					    lists:suffix(".beam", F)],
	    Mods ++ Acc;
	_ ->
	    Acc
    end.

%% Returns: [FunName] of all exported functions of Mod starting with Prefix.
%% Mod = atom()
%% Prefix = string()
functions(Mod, _Prefix) ->
%  FIXME: have to decide which approach is better - all loaded or all in path
%         i, of course, prefer all in path (mbj)
    case beamfile(Mod) of
	{ok, BeamFile} ->
	    case get_exports(BeamFile) of
		{ok, Exports0} ->
		    Exports = Exports0 -- [{"module_info",0},{"module_info",1}],
		    Fns = [Fun || {Fun, _Arity} <- Exports],
		    {ok, ordsets:to_list(ordsets:from_list(Fns))};
		error ->
		    {error, fmt("Can't get export list for ~p", [Mod])}
	    end;
	_ ->
	    {error, fmt("Can't find beam file for ~p", [Mod])}
    end.

%% ----------------------------------------------------------------------
%% Refactoring
%% ----------------------------------------------------------------------

%% @spec free_vars(Text::string()) ->
%%           {ok, FreeVars::[atom()]} | {error, Reason::string()}
%% @equiv free_vars(Text, 1)

free_vars(Text) ->
    free_vars(Text, 1).

%% @spec free_vars(Text::string(), Line::integer()) ->
%%           {ok, FreeVars::[atom()]} | {error, Reason::string()}

free_vars(Text, StartLine) ->
    %% StartLine/EndLine may be useful in error messages.
    {ok, Ts, EndLine} = erl_scan:string(Text, StartLine),
    %%Ts1 = lists:reverse(strip(lists:reverse(Ts))),
    Ts2 = [{'begin', 1}] ++ Ts ++ [{'end', EndLine}, {dot, EndLine}],
    case erl_parse:parse_exprs(Ts2) of
        {ok, Es} ->
            E = erl_syntax:block_expr(Es),
            E1 = erl_syntax_lib:annotate_bindings(E, ordsets:new()),
            {value, {free, Vs}} = lists:keysearch(free, 1,
                                                  erl_syntax:get_ann(E1)),
            {ok, Vs};
        {error, {_Line, erl_parse, Reason}} ->
            {error, fmt("~s", [Reason])}
    end.

strip([{',', _}   | Ts]) -> strip(Ts);
strip([{';', _}   | Ts]) -> strip(Ts);
strip([{'.', _}   | Ts]) -> strip(Ts);
strip([{'|', _}   | Ts]) -> strip(Ts);
strip([{'=', _}   | Ts]) -> strip(Ts);
strip([{'dot', _} | Ts]) -> strip(Ts);
strip([{'->', _}  | Ts]) -> strip(Ts);
strip([{'||', _}  | Ts]) -> strip(Ts);
strip([{'of', _}  | Ts]) -> strip(Ts);
strip(Ts)                -> Ts.

%% ----------------------------------------------------------------------
%% Online documentation
%% ----------------------------------------------------------------------

apropos(RE, false) ->
    apropos(RE);
apropos(RE, true) ->
    fdoc:stop(),
    apropos(RE).

apropos(RE) ->
    fdoc_binaryify(fdoc:get_apropos(RE)).

describe(M, F, A, false) ->
    describe(M, F, A);
describe(M, F, A, true) ->
    fdoc:stop(),
    describe(M, F, A).

describe(M, F, A) ->
%% FIXME using nonm-cached version instead.  remove old code
    fdoc_binaryify(fdoc:describe2(M, F, A)).
%    fdoc_binaryify(fdoc:description(M, F, A)).

%% Converts strings to binaries, for Emacs
fdoc_binaryify({ok, Matches}) ->
    {ok, [{M, F, A, to_bin(Doc)} || {M, F, A, Doc} <- Matches]};
fdoc_binaryify(Other) -> Other.

%% ----------------------------------------------------------------------
%% Argument list snarfing
%% ----------------------------------------------------------------------

%% Given the name of a function we return the argument lists for each
%% of its definitions (of different arities). The argument lists are
%% heuristically derived from the patterns in each clause of the
%% function. We try to derive the most human-meaningful arglist we
%% can.

%% Entry point;
%% Get the argument lists for a function in a module.
%% Return: [Arglist]
%% Arglist = [string()]
get_arglists(ModName, FunName) when list(ModName), list(FunName) ->
    arglists(to_atom(ModName), FunName).

arglists(Mod, Fun) ->
    case get_abst_from_debuginfo(Mod) of
	{ok, Abst} ->
	    case fdecls(to_atom(Fun), Abst) of
		[] -> error;
		Fdecls -> map(fun derive_arglist/1, Fdecls)
	    end;
	error ->
	    case beamfile(Mod) of
		{ok, BeamFile} ->
		    case get_exports(BeamFile) of
			{ok, Exports} ->
			    Funs = [{Fun0, Arity0} || {Fun0, Arity0} <- Exports,
						      Fun0 == Fun],
			    case get_forms_from_src(Mod) of
				{ok, Forms} ->
				    get_arglist_from_forms(Funs, Forms);
				_ ->
				    error
			    end;
			_ ->
			    error
		    end;
		_ ->
		    error
	    end
    end.

%% Find the {function, ...} entry for the named function in the
%% abstract syntax tree.
%% Return: {ok, {function ...}} | error
fdecls(Name, Abst) ->
    {_Exports,Forms} = Abst,
    [Fdecl || Fdecl <- Forms,
	      element(1, Fdecl) == function,
	      element(3, Fdecl) == Name].

%% Get the abstrct syntax tree for `Mod' from beam debug info.
%% Return: {ok, Abst} | error
get_abst_from_debuginfo(Mod) ->
    case beamfile(Mod) of
	{ok, Filename} ->
	    case read_abst(Filename) of
		{ok, Abst} ->
		    {ok, binary_to_term(Abst)};
		error ->
		    error
	    end;
	error ->
	    error
    end.

get_forms_from_src(Mod) ->
    case find_source(Mod) of
	{ok, SrcFName} ->
	    epp_dodger:parse_file(SrcFName);
	_ ->
	    error
    end.

%% Return the name of the beamfile for Mod.
beamfile(Mod) ->
    case code:which(Mod) of
	File when list(File) ->
	    {ok, File};
	_ ->
	    error
    end.

%% Read the abstract syntax tree from a debug info in a beamfile.
read_abst(Beam) ->
    case beam_lib:chunks(Beam, ["Abst"]) of
	{ok, {_Mod, [{"Abst", Abst}]}} when Abst /= <<>> ->
	    {ok, Abst};
	_ -> error
    end.

%% Derive a good argument list from a function declaration.
derive_arglist(Fdecl) ->
    Cs = clauses(Fdecl),
    Args = merge_args(map(fun clause_args/1, Cs)),
    map(fun to_list/1, Args).

clauses({function, _Line, _Name, _Arity, Clauses}) -> Clauses.
clause_args({clause, _Line, Args, _Guard, _Body}) ->
    map(fun argname/1, Args).

%% Return a name for an argument.
%% The result is either 'unknown' (meaning "nothing meaningful"), a
%% string (meaning a type-description), or another atom (meaning a
%% variable name).
argname({var, _Line, V}) ->
    case to_list(V) of
	"_"++_ -> unknown;
	_      -> V
    end;
argname({Type, _Line, _})          -> to_list(Type)++"()";
argname({nil, _Line})              -> "list()";
argname({cons, _Line, _Car, _Cdr}) -> "list()";
argname({match, _Line, LHS, _RHS}) -> argname(LHS);
argname(_)                         -> unknown.

%% Merge the arglists of several clauses together to create the best
%% single one we can.
merge_args(Arglists) ->
    map(fun best_arg/1, transpose(Arglists)).

%% Return the heuristically best argument description in Args.
best_arg(Args) ->
    foldl(fun best_arg/2, hd(Args), tl(Args)).

%% Return the better of two argument descriptions.
%% 'unknown' useless, type description is better, variable name is best.
best_arg(unknown, A2)          -> A2;
best_arg(A1, unknown)          -> A1;
best_arg(A1, A2) when atom(A1),atom(A2) ->
    %% ... and the longer the variable name the better
    case length(to_list(A2)) > length(to_list(A1)) of
	true -> A2;
	false -> A1
    end;
best_arg(A1, _A2) when atom(A1) -> A1;
best_arg(_A1, A2)               -> A2.

%% transpose([[1,2],[3,4],[5,6]]) -> [[1,3,5],[2,4,6]]
transpose([[]|_]) ->
    [];
transpose(L) ->
    [[hd(X) || X <- L] | transpose([tl(X) || X <- L])].

get_arglist_from_forms(Funs, Forms) ->
    map(fun({Fun, Arity}) -> src_args(Forms, to_atom(Fun), Arity) end, Funs).

src_args(_, _, 0) -> [];
src_args([{tree,function,_,{function,{tree,atom,_,Func}, Clauses}} = H | T],
	 Func, Arity) ->
    case erl_syntax_lib:analyze_function(H) of
	{_, Arity} ->
	    ArgsL0 = [Args || {tree, clause, _, {clause,Args,_,_}} <- Clauses],
	    ArgsL1 = [map(fun argname/1, Args) || Args <- ArgsL0],
	    Args = merge_args(ArgsL1),
	    map(fun to_list/1, Args);
	_ ->
	    src_args(T, Func, Arity)
    end;
src_args([_ |T], Func, Arity) ->
    src_args(T, Func, Arity).
%% if we get to [] we have a serious error

get_exports(BeamFile) ->
    case beam_lib:chunks(BeamFile, ["Atom", "ExpT"]) of
	{ok, {_, [{"Atom", AtomBin}, {"ExpT", ExpBin}]}} ->
	    Atoms = beam_disasm_atoms(AtomBin),
	    {ok, beam_disasm_exports(ExpBin, Atoms)};
	_ ->
	    error
    end.
%%-----------------------------------------------------------------------
%% BEGIN Code snitched from beam_disasm.erl; slightly modified
%%-----------------------------------------------------------------------
beam_disasm_atoms(AtomTabBin) ->
    {_NumAtoms,B} = get_int(AtomTabBin),
    disasm_atoms(B).

disasm_atoms(AtomBin) ->
    disasm_atoms(to_list(AtomBin),1).

disasm_atoms([Len|Xs],N) ->
    {AtomName,Rest} = get_atom_name(Len,Xs),
    [{N,AtomName}|disasm_atoms(Rest,N+1)];
disasm_atoms([],_) ->
    [].

get_atom_name(Len,Xs) ->
    get_atom_name(Len,Xs,[]).

get_atom_name(N,[X|Xs],RevName) when N > 0 ->
    get_atom_name(N-1,Xs,[X|RevName]);
get_atom_name(0,Xs,RevName) ->
    { lists:reverse(RevName), Xs }.

beam_disasm_exports(none, _) -> none;
beam_disasm_exports(ExpTabBin, Atoms) ->
    {_NumAtoms,B} = get_int(ExpTabBin),
    disasm_exports(B,Atoms).

disasm_exports(Bin,Atoms) ->
    resolve_exports(collect_exports(to_list(Bin)),Atoms).

collect_exports([F3,F2,F1,F0,A3,A2,A1,A0,_L3,_L2,_L1,_L0|Exps]) ->
    [{i32([F3,F2,F1,F0]),  % F = function (atom ID)
      i32([A3,A2,A1,A0])}  % A = arity (int)
%      i32([L3,L2,L1,L0])}  % L = label (int)
     |collect_exports(Exps)];
collect_exports([]) ->
    [].

resolve_exports(Exps,Atoms) ->
    [ {lookup_key(F,Atoms), A} || {F,A} <- Exps ].

lookup_key(Key,[{Key,Val}|_]) ->
    Val;
lookup_key(Key,[_|KVs]) ->
    lookup_key(Key,KVs);
lookup_key(Key,[]) ->
    exit({lookup_key,{key_not_found,Key}}).

i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_int(B) ->
    {I, B1} = split_binary(B, 4),
    {i32(to_list(I)), B1}.
%%-----------------------------------------------------------------------
%% END Code snitched from beam_disasm.erl
%%-----------------------------------------------------------------------


%%-----------------------------------------------------------------
%% Call graph
%%-----------------------------------------------------------------
-define(SERVER, distel_callers).

%% Ret: [{M,F,A}], M = F = binary()
who_calls(M, F, A) ->
    [{fmt("~p", [Mod]), fmt("~p", [Fun]), Aa, Line}
     || {Mod,Fun,Aa,Line} <- calls_to(M, F, A)].

%% Ret: [{M,F,A,Line}] of callers to M:F/A
calls_to(M, F, A) ->
    ensure_started(),
    call({calls_to, {M,F,A}}).

%% Ret: [{{F,A}, [{CM,CF,CA,Line}]}] of calls from M
calls_from(M) ->
    ensure_started(),
    call({calls_from, M}).

rebuild() ->
    stop(),
    ensure_started().

stop() ->
    catch exit(?SERVER, shutdown),
    ok.

ensure_started() ->
    case whereis(?SERVER) of
	undefined ->
	    Pid = proc_lib:start(?MODULE, start, []),
	    register(?SERVER, Pid);
	_ ->
	    ok
    end.

call(Msg) ->
    ?SERVER ! {Msg, self()},
    receive {?SERVER, Reply} -> Reply end.

%% This is the process that keeps data per file in the code path.
%% It will handle the case that a file is added/removed/updated, but
%% it will not rebuild the code path.  So if the code path changes, it needs
%% to rebuild the entire cache.
start() ->
    Applies = get_applies(),
    Callers = init_callers(Applies),
    proc_lib:init_ack(self()),
    loop(Applies, Callers).

loop(Applies, Callers0) ->
    receive
	{{calls_to, MFA}, From} ->
	    Callers1 = refresh_callers(Callers0, Applies),
	    Res = find_calls_to(Callers1, MFA),
	    From ! {?SERVER, Res},
	    distel:loop(Applies, Callers1);
	{{calls_from, M}, From} ->
	    Callers1 = refresh_callers(Callers0, Applies),
	    Res = find_calls_from(Callers1, M),
	    From ! {?SERVER, Res},
	    distel:loop(Applies, Callers1)
    end.
	
%% Callers = [{Dir, [{Mod, Timestamp, Calls}]}]
%% Calls = [{F,Arity}, [{{CalledM, CalledF, CalledArity}, Line}]]
init_callers(Applies) ->
    Dirs = get_code_path(),
    InitState = [{Dir, []} || Dir <- Dirs],
    refresh_callers(InitState, Applies).

find_calls_to(Callers, MFA) ->
    foldl(fun({_Dir, Mods}, Ac1) ->
		  foldl(fun({M, _Timestamp, Calls}, Ac2) ->
				foldl(fun({{F, A}, Called}, Ac3) ->
					      foldl(fun({CMFA, Line}, Ac4)
						       when CMFA == MFA ->
							    [{M,F,A,Line}|Ac4];
						       (_, Ac4) ->
							    Ac4
						    end, Ac3, Called)
				      end, Ac2, Calls)
			end, Ac1, Mods)
	  end, [], Callers).

find_calls_from([{_Dir, Mods} | T], M) ->
    case find_calls_from_mod(Mods, M) of
	{ok, Calls} -> Calls;
	error       -> find_calls_from(T, M)
    end;
find_calls_from([], _) ->
    [].

find_calls_from_mod([{M, _Timestamp, Calls} | _], M) ->
    {ok, Calls};
find_calls_from_mod([_ | T], M) ->
    find_calls_from_mod(T, M);
find_calls_from_mod([], _M) ->
    error.

refresh_callers(Callers, _Applies) ->
    map(fun refresh_dir/1, Callers).

ts(F) ->
    {ok, FI} = file:read_file_info(F),
    FI#file_info.mtime.

refresh_dir({Dir, Mods}) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    FsMods0 = [{to_atom(basename(F, ".beam")), ts(Dir++"/"++F)} ||
			  F <- Files,
			  lists:suffix(".beam", F)],
	    FsMods = lists:keysort(1, FsMods0),
	    {Dir, refresh_mods(Mods, FsMods)};
	_ ->
	    {Dir, []}
    end.

refresh_mods([{M, Ts, _Calls} = H | T1], [{M, Ts} | T2]) -> % not modified
    [H | refresh_mods(T1, T2)];
refresh_mods([{M, _, _} | T1], [{M, Ts} | T2]) -> % modified
    [{M, Ts, calls(M)} | refresh_mods(T1, T2)];
refresh_mods([{M1, _, _} | _] = L1, [{M2, Ts} | T2]) when M1 > M2 -> % M2 new
    [{M2, Ts, calls(M2)} | refresh_mods(L1, T2)];
refresh_mods([_ | T1], L2) -> % M1 removed
    refresh_mods(T1, L2);
refresh_mods([], [{M2, Ts} | T2]) -> % M2 new
    [{M2, Ts, calls(M2)} | refresh_mods([], T2)];
refresh_mods(_, []) ->
    [].


%% FIXME: make configurable (and complete in otp case)
get_applies() ->
    [{erlang, apply, 3},
     {erlang, spawn, 3},
     {erlang, spawn_link, 3},
     {proc_lib, start_link, 3},
     {rpc, call, 4},
     {net_ctrl, multi_call, 3}].

%% FIXME: make pruning configurable, e.g. remove all otp apps
get_code_path() ->
    code:get_path().

%% Ret: [{F,Arity}, [{{CalledM, CalledF, CalledArity}, Line}]]
calls(Mod) ->
    Applies = get_applies(),
    case get_abst_from_debuginfo(Mod) of
	{ok, {_Exports, Forms}} ->
	    io:format("adding (debug_info): ~p\n", [Mod]),
	    calls(Mod, Forms, Applies);
	error ->
	    io:format("adding (source): ~p\n", [Mod]),
	    case get_forms_from_src(Mod) of
		{ok, Forms} ->
		    calls(Mod, Forms, Applies);
		error ->
		    io:format("not found: ~p\n", [Mod]),
		    []
	    end
    end.

calls(ThisMod, Tree, Applies) ->
    {_, Calls} =
	lists:foldl(fun(T, Acc) ->
			    do_funs(T, Acc, ThisMod, Applies)
		    end, {[], []}, Tree),
    Calls.

do_funs(Tree, {Imports, Calls} = Acc, ThisM, Applies) ->
    case erl_syntax:type(Tree) of
	attribute ->
	    case catch erl_syntax_lib:analyze_attribute(Tree) of
		{import, {Mod, Funs}} ->
		    MFAs = [{Fun, Mod} || Fun <- Funs],
		    {MFAs ++ Imports, Calls};
		_ ->
		    Acc
	    end;
	function ->
	    F = erl_syntax:atom_value(erl_syntax:function_name(Tree)),
	    A = erl_syntax:function_arity(Tree),
	    Called = erl_syntax_lib:fold(
		       fun(N,Ac) ->
			       do_applications(N,Ac,ThisM,F,A,Imports,Applies)
		       end, [], Tree),
	    {Imports, [{{F,A}, Called} | Calls]};
	_ ->
	    Acc
    end.

%% FIXME: handle fun <name>/<arity> syntax.
do_applications(Tree, Acc, ThisM,ThisF, ThisA,  Imports, Applies) ->
    case erl_syntax:type(Tree) of
	application ->
	    Line = erl_syntax:get_pos(Tree),
	    case catch erl_syntax_lib:analyze_application(Tree) of
		{'EXIT', _X} -> % needed b/c analyze_app may throw syntax_error
		    Acc;
		{M, {F, A}} ->
		    add_call(M, F, A, Line, Tree, Acc, Applies);
		{ThisF, ThisA} -> % direct recursive call to our selves, ignore
		    Acc;
		{F, A} ->
		    case lists:keysearch({F, A}, 1, Imports) of
			{value, {_, M}} ->
			    add_call(M, F, A, Line, Tree, Acc, Applies);
			_ ->
			    case is_bif(F, A) of
				{true, M} ->
				    add_call(M, F, A, Line, Tree, Acc, Applies);
				false ->
				    add_call(ThisM, F, A, Line, Tree, Acc, 
					     Applies)
			    end
		    end;
		_X ->
		    Acc
	    end;
	_ ->
	    Acc
    end.

add_call(M, F, A, Line, Tree, Calls, Applies) ->
    MFA = {M,F,A},
    case lists:member(MFA, Applies) of
	true ->
	    case application(Tree, A) of
		{true, CalledMFA} ->
		    [{MFA, Line}, CalledMFA | Calls];
		false ->
		    [{MFA, Line} | Calls]
	    end;
	false ->
	    [{MFA, Line} | Calls]
    end.

application(Tree, 3) ->
    [M, F, Args] = erl_syntax:application_arguments(Tree),
    application(M,F,Args);
application(Tree, 4) ->
    [_Node, M, F, Args] = erl_syntax:application_arguments(Tree),
    application(M,F,Args).

application(M,F,Args) ->
    case {erl_syntax:type(M), erl_syntax:type(F)} of
	{atom, atom} ->
	    Arity = case erl_syntax:type(Args) of
			list -> case catch erl_syntax:list_length(Args) of
				    N when integer(N) -> N;
				    _ -> -1 % e.g. [A,B | T]
				end;
			nil -> 0;
			_ -> -1
		    end,
	    {true, {erl_syntax:atom_value(M),
		    erl_syntax:atom_value(F),
		    Arity,
		    erl_syntax:get_pos(M)}};
	_ ->
	    false
    end.
	    
%% isn't there a better way to check this???
is_bif(F, A) ->
    case erl_bifs:is_bif(erlang, F, A) of
	true ->
	    {true, erlang};
	false ->
	    case erl_bifs:is_bif(math, F, A) of
		true ->
		    {true, math};
		false ->
		    false
	    end
    end.
			  
%%-----------------------------------------------------------------
%% Call graph mode:
%% 
%% Callers of foo:bar/2
%%   + baz:doit/3
%%     foo:baz/0
%%   - baz:foo/2
%%       +  ggg:foo/2
%%          hhh:foo/0
%%
%% Keys:
%%   'ret' on a '+' line expands
%%   'ret' on a '-' line collapses
%%    ?? jump to definition in other buffer
%%-----------------------------------------------------------------
