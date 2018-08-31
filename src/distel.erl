%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% File    : distel.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Helper functions to be called from Emacs.
%%%
%%% Created : 18 Mar 2002 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(distel).
-author('luke@bluetail.com').

%% API
-export([rpc_entry/3, reload_modules/0, reload_module/2,
         eval_expression/1, find_source/1]).
-export([process_list/0, process_info_item/2, process_summary/1,
         process_summary_and_trace/2]).
-export([fprof/1, fprof/3]).
-export([debug_toggle/2, debug_add/1, break_toggle/2, break_delete/2,
         break_add/2, break_restore/1, debug_subscribe/1, debug_attach/2]).
-export([modules/1, functions/2, xref_modules/1, xref_functions/2,
         rebuild_completions/0]).
-export([free_vars/1]).
-export([apropos/1, describe/2, arglists/2]).
-export([xref_callgraph/1, who_calls/3, rebuild_callgraph/0]).

-include_lib("kernel/include/file.hrl").

-import(lists, [any/2,
                append/1,
                duplicate/2,
                filter/2,
                flatten/1,
                foldl/3,
                foreach/2,
                keysearch/3,
                map/2,
                member/2,
                prefix/2,
                reverse/1,
                sort/1,
                usort/1]).

-import(filename, [dirname/1, join/1, basename/2]).

%% internal use
-export([gl_proxy/1, tracer_init/2, debug_subscriber_init/2,
         attach_init/2, attach_loop/1]).

fmt(F, A) -> to_bin(io_lib:fwrite(F,A)).

to_bin(X) -> list_to_binary(to_list(X)).
to_atom(X) -> list_to_atom(to_list(X)).

to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_integer(X)-> integer_to_list(X);
to_list(X) when is_float(X)  -> float_to_list(X);
to_list(X) when is_atom(X)   -> atom_to_list(X);
to_list(X) when is_list(X)   -> X.              %Assumed to be a string

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
    to_atom(fmt("distel_gl_for_~p", [Pid])).

gl_proxy(GL) ->
    receive
        {io_request, From, ReplyAs, {put_chars, C}} ->
            GL ! {put_chars, C},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, unicode, C}} ->
            GL ! {put_chars, C},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, M, F, A}} ->
            GL ! {put_chars, flatten(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, unicode,M,F,A}} ->
            GL ! {put_chars, flatten(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {get_until, _, _, _}} ->
            %% Input not supported, yet
            From ! {io_reply, ReplyAs, eof};
        Unknown ->
            GL ! {put_chars, to_bin(io_lib:format("GL DEBUG: ~p~n", [Unknown]))}
    end,
    gl_proxy(GL).

%% ----------------------------------------------------------------------
%%% reload all modules that are out of date
%%% compare the compile time of the loaded beam and the beam on disk
reload_modules() ->
    T = fun(L) -> [X || X <- L, element(1,X) =:= time] end,
    Tm = fun(M) -> T(M:module_info(compile)) end,
    Tf = fun(F) -> {ok,{_,[{_,I}]}}=beam_lib:chunks(F,[compile_info]),T(I) end,
    Load = fun(M) -> c:l(M),M end,

    [Load(M) || {M,F} <- code:all_loaded(), is_beamfile(F), Tm(M)<Tf(F)].

is_beamfile(F) ->
    ok == element(1,file:read_file_info(F)) andalso
        ".beam" == filename:extension(F).

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
                        case filter(fun is_beam/1, Beams) of
                            [] -> {error, R};
                            [Beam|_] ->
                                code:add_patha(dirname(Beam)),
                                c:l(to_atom(Mod))
                        end;
                false ->
                    {error,modname_and_filename_differs}
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
            {ok, fmt("~p", [V])};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

parse_expr(S) ->
    {ok, Scan, _} = erl_scan:string(S),
    erl_parse:parse_exprs(Scan).

find_source(Mod) ->
  case beamfile(Mod) of
    {ok, BeamFName} ->
      try {ok,guess_source_file(Mod, BeamFName)}
      catch
        nothing -> {error,fmt("Can't find source for ~p", [BeamFName])};
        several -> {error,fmt("Several possible sources for ~p", [BeamFName])};
        _:R -> {error,fmt("Couldnt guess source ~p ~p",[BeamFName,R])}
      end;
    error ->
      {error, fmt("Can't find module '~p' on ~p", [Mod, node()])}
  end.

beamfile(Mod) ->
    case code:which(Mod) of
        File when is_list(File) ->
            {ok, File};
        _ ->
            error
    end.

%% Ret: AbsName | throw(Reason)
%% Ret: AbsName | throw(Reason)
guess_source_file(Mod, BeamFName) ->
  Erl = to_list(Mod) ++ ".erl",
  Dir = dirname(BeamFName),
  DotDot = dirname(Dir),
  try_srcs([src_from_beam(Mod),
            filelib:wildcard(join([DotDot, "**", Erl]))]).

try_srcs([]) -> throw(nothing);
try_srcs(["" | T]) -> try_srcs(T);
try_srcs([H | T]) ->
  case filelib:wildcard(H) of
    [File] ->
      case filelib:is_regular(File) of
        true -> File;
        false-> try_srcs(T)
      end;
    [] -> try_srcs(T);
    _Multi -> throw(several)
  end.

src_from_beam(Mod) ->
  try
    case Mod:module_info(compile) of
      [] -> int:file(Mod);
      CompInfo -> {_,{_,Src}} = keysearch(source,1,CompInfo), Src
    end
  catch _:_ -> ""
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
            to_list(fmt("~p", [Pid]))
    end.

initial_call(Pid) ->
    {initial_call, {M, F, A}} = process_info(Pid, initial_call),
    to_list(fmt("~s:~s/~p", [M, F, A])).

reductions(Pid) ->
    {reductions, NrReds} = process_info(Pid, reductions),
    to_list(NrReds).

messages(Pid) ->
    {messages, MsgList} = process_info(Pid, messages),
    to_list(length(MsgList)).

iformat(A1, A2, A3, A4) ->
    fmt("~-21s ~-33s ~12s ~8s~n",[A1,A2,A3,A4]).

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
    Text = [fmt("~-20w: ~w~n", [Key, Value])
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
        Trace when is_tuple(Trace),
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

fprof_tag({M,F,A}) when is_integer(A) ->
    to_atom(fmt("~p:~p/~p", [M,F,A]));
fprof_tag({M,F,A}) when is_list(A) ->
    fprof_tag({M,F,length(A)});
fprof_tag(Name) when is_atom(Name) ->
    Name.

fprof_mfa({M,F,A}) -> [M,F,A];
fprof_mfa(_)       -> undefined.

fprof_tag_name(X) -> fmt("~s", [fprof_tag(X)]).

fprof_text({Name, Cnt, Acc, Own}) ->
    fmt("~s~p\t~.3f\t~.3f\n",
        [pad(50, fprof_tag_name(Name)), Cnt, Acc, Own]).

fprof_tags(C) -> [fprof_tag(Name) || {Name,_,_,_} <- C].

fprof_beamfile({M,_,_}) ->
    case code:which(M) of
        Fname when is_list(Fname) ->
            to_bin(Fname);
        _ ->
            undefined
    end;
fprof_beamfile(_) -> undefined.

pad(X, A) when is_atom(A) ->
    pad(X, to_list(A));
pad(X, S) when length(S) < X ->
    S ++ duplicate(X - length(S), $\s);
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
    case is_interpreted(Mod) of
        true -> int:n(Mod), uninterpreted;
        false-> int_i(Mod, Filename), interpreted
    end.

debug_add(Modules) ->
    foreach(fun([Mod, Filename]) -> assert_int(Mod, Filename) end, Modules),
    ok.

break_toggle(Mod, Line) ->
    case any(fun({Point,_}) -> Point == {Mod,Line} end,
             int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line),
            disabled;
        false ->
            ok = int:break(Mod, Line),
            enabled
    end.

break_delete(Mod, Line) ->
    case any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line);
        false ->
            ok
    end.

break_add(Mod, Line) ->
    case any(fun({Point,_}) -> Point == {Mod,Line} end,
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

%% Returns: {InterpretedMods, Breakpoints, [{Pid, Text}]}
%%          InterpretedMods = [[Mod, File]]
%%          Breakpoints     = [{Mod, Line}]
debug_subscribe(Pid) ->
    %% NB: doing this before subscription to ensure that the debugger
    %% server is started (int:subscribe doesn't do this, probably a
    %% bug).
    Interpreted = [[Mod, erl_from_mod(Mod)] || Mod <- int_interpreted()],
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
            Pid ! {int, {interpret, Mod, erl_from_mod(Mod)}};
        Msg ->
            Pid ! Msg
    end,
    debug_subscriber(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% re-implementation of some int.erl functions

erl_from_mod(Mod) ->
    try {ok,{Mod,[{_,Bin}]}} = beam_lib:chunks(code:which(Mod),["CInf"]),
        {_,{_,ErlFile}} = lists:keysearch(source,1,binary_to_term(Bin)),
        ErlFile
    catch _:_ -> ""
    end.


assert_int(Mod,Filename) ->
    case is_interpreted(Mod) of
        true -> ok;
        false-> int_i(Mod,Filename)
    end.

int_i(Mod,Srcfile) ->
    try Beamfile = code:which(Mod),
        case beam_lib:chunks(Beamfile, [abstract_code,exports]) of
            {ok,{Mod,[{abstract_code,no_abstract_code},_]}} ->
                throw(no_debug_info);
            {ok,{Mod,[{abstract_code,Abst},{exports,Exps}]}} ->
                int_i(Mod, Exps, Abst, Srcfile, Beamfile)
        end
    catch _:R -> throw(R)
    end.


int_i(Mod, Exps, Abst, Srcfile, Beamfile) ->
    code:purge(Mod),
    erts_debug:breakpoint({Mod,'_','_'}, false),
    {module,Mod} = code:load_abs(filename:rootname(Beamfile),Mod),
    {ok, SrcBin} = file:read_file(Srcfile),
    {ok, BeamBin} = file:read_file(Beamfile),
    MD5 = code:module_md5(BeamBin),
    Bin = term_to_binary({interpreter_module,Exps,Abst,SrcBin,MD5}),
    {module, Mod} = dbg_iserver:safe_call({load, Mod, Srcfile, Bin}),
    true = erts_debug:breakpoint({Mod,'_','_'}, true) > 0.

is_interpreted(Mod) ->
    lists:member(Mod,int:interpreted()).

int_interpreted() ->
    int:interpreted().

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
        {Meta, Status} when is_atom(Status) ->
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
        {NPos, Mod, Line} ->               %OTP R9
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
        {NPos, Mod, Line} ->               %OTP R9
            attach_goto(Att#attach{stack={NPos,Max}},{Mod,Line});
        bottom ->
            attach_goto(Att#attach{stack={Max, Max}},Att#attach.where)
    end;
attach_meta_cmd({get_binding, Var}, Att = #attach{stack={Pos,_Max}}) ->
    Bs = int:meta(Att#attach.meta, bindings, Pos),
    case keysearch(Var, 1, Bs) of
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

modules(Prefix) ->
    case distel_html_doc:fetch_mods(Prefix) of
        [] -> xref_modules(Prefix);
        Ms -> {ok, [get_module(M) || M <- Ms]}
    end.

functions(Mod, Prefix) ->
    case distel_html_doc:fetch_fas(Mod, Prefix) of
        [] -> xref_functions(Mod, Prefix);
        FAs -> {ok, [get_function(FA) || FA <- FAs]}
    end.

%% Entry point;
%% Get the argument lists for a function in a module.
%% Return: ["A1, A2, ...", "B1, B2, ..."]
arglists(Mod, Fun) ->
    case distel_html_doc:fetch_fas(Mod, Fun) of
        [] -> {ok, []};
        FAs -> {ok, [get_arglist(FA) || FA <- FAs]}
    end.

describe(Mod, Fun) ->
    describe(Mod, Fun, all).

%% returns ["fun(A1, A2,...) -> Return"]
describe(Mod, Fun, _Arity) ->
    case distel_html_doc:fetch_fas(Mod, Fun) of
        [] -> {ok, []};
        FAs -> {ok, FAs}
    end.

%% returns "mod:"
get_module(M) ->
    <<M/binary, ":">>.

%% FA is "fun(A1, A2,..) -> Ret"
%% we return "fun("
get_function(FA) ->
    Mod = string:trim(hd(re:split(FA, "\\("))),
    <<Mod/binary, "(">>.

%% FA is "fun(A1, A2,..) -> Ret"
%% we return "A1, A2, .."
get_arglist(FA) ->
    [_, Args|_] = re:split(FA, "\\(|\\)"),
    [Args].

xref_completions(F,A) ->
    fun(server) -> distel_completions;
       (opts)   -> [{xref_mode, modules}];
       (otp)    -> true;
       (query_) -> to_list(fmt(F,A))
    end.

rebuild_completions() ->
    xref_rebuild(xref_completions("",[])).

%% Returns: [ModName] of all modules starting with Prefix.
%% ModName = Prefix = string()
xref_modules(Prefix) ->
    case xref_query(xref_completions('"~s.*" : Mod', [to_list(Prefix)])) of
        {ok, Mods} -> {ok, [to_list(Mod) || Mod <- Mods]};
        _          -> {error, fmt("Can't find any matches for ~p", [Prefix])}
    end.

%% Returns: [FunName] of all exported functions of Mod starting with Prefix.
%% Mod = atom()
%% Prefix = string()
xref_functions(Mod, Prefix) ->
    case xref_query(xref_completions('(X+B) * ~p:"~s.*"/_', [Mod, Prefix])) of
        {ok, Res} -> {ok,usort([to_list(F) || {_M, F, _A}<-Res])};
        _         -> {error, fmt("Can't find module ~p", [Mod])}
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
    Ts2 = [{'begin', 1}] ++ Ts ++ [{'end', EndLine}, {dot, EndLine}],
    case erl_parse:parse_exprs(Ts2) of
        {ok, Es} ->
            E = erl_syntax:block_expr(Es),
            E1 = erl_syntax_lib:annotate_bindings(E, ordsets:new()),
            {value, {free, Vs}} = keysearch(free, 1, erl_syntax:get_ann(E1)),
            {ok, Vs};
        {error, {_Line, erl_parse, Reason}} ->
            {error, fmt("~s", [Reason])}
    end.

%% ----------------------------------------------------------------------
%% Online documentation
%% ----------------------------------------------------------------------

apropos(_RE) ->
    [].

%%-----------------------------------------------------------------
%% Call graph
%%-----------------------------------------------------------------

xref_callgraph(A) ->
    F = to_list(fmt("(XXL)(Lin)(E || ~p)",[A])),
    fun(server)-> distel_callgraph;
       (opts) -> [];
       (otp) -> false;
       (query_)-> F
    end.

%% Ret: [{M,F,A,Line}], M = F = list()
who_calls(Mm, Fm, Am) ->
    try XREF=xref_callgraph({Mm,Fm,Am}),
        {ok, Calls} = xref_query(XREF),
        append([[xform(M,F,A,L) || L <- Ls] || {{{{M,F,A},_},_}, Ls} <- Calls])
    catch _:_ ->
        {error,{who_calls,"try rebuilding callgraph (C-c C-d W)"}}
    end.

xform(M, F, A, L) ->
  {to_list(M),to_list(F),A,L}.

rebuild_callgraph() ->
    xref_rebuild(xref_callgraph("")).
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

xref_query(XREF) ->
    ensure_started(XREF),
    xref:q(XREF(server), XREF(query_)).

xref_rebuild(XREF) ->
    stop(XREF),
    ensure_started(XREF).

ensure_started(XREF) ->
    case whereis(XREF(server)) of
        undefined -> start(XREF);
        _         -> xref:update(XREF(server))
    end.

stop(XREF) ->
    case whereis(XREF(server)) of
        undefined -> ok;
        _         -> xref:stop(XREF(server))
    end.

start(XREF) ->
    xref:start(XREF(server), XREF(opts)),
    xref:set_default(XREF(server), builtins, true),
    F = fun(D) -> xref:add_directory(XREF(server), D) end,
    foreach(F, get_code_path(XREF)).

get_code_path(XREF) ->
    case XREF(otp) of
        false->
            Root = code:root_dir(),
            [Dir || Dir <- code:get_path(), not prefix(Root,Dir)];
        true ->
            code:get_path()
    end.
