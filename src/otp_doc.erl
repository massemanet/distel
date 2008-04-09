%% -*- erlang-indent-level: 2 -*-
%%% Created :  6 Mar 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('otp_doc').
-author('Mats Cronqvist').

-define(is_str(S),is_integer(hd(S));S==[]).
%% --------------------------------------------------------------------------
%% gen_server boilerplate
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2, handle_info/2, 
         init/1, terminate/2, code_change/3]).

%% --------------------------------------------------------------------------
% API - these run in the shell
-export([start/0,stop/0]).
-export([get/4]).
-export([firefox/1,firefox/2,firefox/3]).
-export([sig/1,sig/2,sig/3]).

stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    _ -> gen_server:call(?MODULE,stop)
  end.

start() -> start([]).
start(Props) -> assert(Props).

get(What,M,F,A) when is_atom(What),is_atom(M),is_atom(F),is_integer(A) ->
  get(What,to_list(M),to_list(F),A);
get(sig,M,F,A) when ?is_str(M), ?is_str(F), is_integer(A) ->
  assert([]),
  gen_server:call(?MODULE,{sig,M,F,to_list(A)});
get(link,M,F,A) when ?is_str(M), ?is_str(F), is_integer(A) ->
  assert([]),
  gen_server:call(?MODULE,{link,M,F,to_list(A)}).

sig(M) -> sig(M,'').
sig(M,F) -> sig(M,F,-1).
sig(M,F,A) when is_atom(M), is_atom(F), is_integer(A) -> get(sig,M,F,A).

firefox(M) -> firefox(M,'').
firefox(M,F) -> firefox(M,F,-1).
firefox(M,F,A) -> ffx(get(link,M,F,A)).
  
ffx(Link) when ?is_str(Link) -> os:cmd("firefox "++Link);
ffx(MFAs) -> [io_str("~s:~s/~s",[M,F,A]) || {M,F,A} <- MFAs].

assert(Props) ->
  case whereis(?MODULE) of
    undefined -> gen_server:start({local,?MODULE},?MODULE,Props,[]);
    Pid -> {ok,Pid}
  end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(I) when is_integer(I)-> integer_to_list(I).

%% --------------------------------------------------------------------------
%% implementation - runs in the server
-record(state,{root_dir,prot=file,delim="/"}).

%% gen_server callbacks
init(Props) -> 
  Dir =  proplists:get_value(root_dir, Props, code:root_dir()),
  Prot = proplists:get_value(prot, Props, file),
  ets:new(?MODULE,[named_table,ordered_set]),
  html_index(Prot,Dir),
  {ok,#state{root_dir=Dir,prot=Prot}}.

terminate(_,_) -> ok.
code_change(_,State,_) -> {ok,State}.
handle_cast(_In,State) -> {noreply,State}.
handle_info(_In,State) -> {noreply,State}.

handle_call(stop,_From,State) -> 
  {stop,normal,ok,State};
handle_call({sig,M,F,A},_From,State) -> 
  {reply,handle_sig(M,F,A,State),State};
handle_call({link,M,F,A},_From,State) -> 
  {reply,handle_link(M,F,A,State),State}.

%% --------------------------------------------------------------------------
handle_sig(Mo,Fu,Aa,_State) ->
  try
    [get_sig(M,F,A) || {M,F,A} <- matching_mfas(Mo, Fu, Aa)]
  catch
    _:_ -> no_match
  end.

handle_link(Mo,Fu,Aa,State) ->
  try 
    case matching_mfas(Mo, Fu, Aa) of
      [{M,F,A}] -> format_link(M, F, A, State);
      MFAs -> 
	case lists:usort([M || {M,_F,_A} <- MFAs]) of
	  [M] -> e_get({file,M});
	  _ -> MFAs
	end
    end
  catch
    no_data -> []
  end.

format_link(M, F, A, State) ->
  io_str("~w://~s#~s~s~s",
	 [State#state.prot,
	  e_get({file,M}),
	  F,
	  State#state.delim,
	  A]).

get_sig(M,F,A) ->
  Sig = e_get({{sig,M,F},A}),
  io_str("~s:~s",[M,Sig]).

matching_mfas(Mo, Fu, Aa) ->
    Ms = all_prefix_keys({file,Mo}),
    MFs = lists:append([all_fs(M,Fu) || M <- Ms]),
    lists:append([[{M,F,A} || A <- which_a(M,F,Aa)] || {M,F} <- MFs]).

all_fs(Mo,Fu) ->
  maybe_cache(Mo),
  [{Mo,F} || F <- all_prefix_keys({{as,Mo},Fu})].

all_prefix_keys({Tag,X}) ->
  case ets:member(?MODULE,{Tag,X}) of
    true -> [X|all_prefix_keys({Tag,X},ets:next(?MODULE,{Tag,X}))];
    false-> all_prefix_keys({Tag,X},ets:next(?MODULE,{Tag,X}))
  end.

all_prefix_keys({Tag,X0},{Tag,X}) ->
  case lists:prefix(X0,X) of
    true -> [X|all_prefix_keys({Tag,X0},ets:next(?MODULE,{Tag,X}))];
    false-> []
  end;
all_prefix_keys(_,_) ->
  [].

which_a(M,F,"-1") -> 
  e_get({{as,M},F});
which_a(M,F,A) ->
  case lists:member(A,e_get({{as,M},F})) of
    true -> [A];
    false-> []
  end.

%% --------------------------------------------------------------------------
%% read the index file
%% store name of html file in {Mod,file}
html_index(file,Dir) ->
  fold_file(curry(fun lines/3,Dir),[],filename:join([Dir,doc,man_index.html])).

lines(Line,_,Dir) ->
  case string:tokens(Line, "<> \"") of
    ["TD", "A", "HREF=","../"++Href, M|_] -> 
      case filename:basename(Href,".html") of
	"index" -> ok;
	M -> e_set({file,M}, filename:join([Dir,Href]))
      end;
    _ -> ok
  end.

%% --------------------------------------------------------------------------
%% read a module's html file
%% store the function names in {Mod,fs}, the arities in {M,F,as} and the 
%% function signature in {M,F,A,sig}

maybe_cache(M) ->
  try e_get({fs,M}) 
  catch 
    no_data -> cache_funcs(M)
  end.

cache_funcs(M) ->
  e_set({fs,M},[]),
  fold_file(curry(fun funcsf/3,M), [], e_get({file,M})).

funcsf(Line,A,M) ->
  case string:tokens(Line++A,"<>\"") of
    ["P","A NAME=",FA,"STRONG","CODE",Sig,"/CODE","/STRONG","/A","BR"|_] ->
      a_line(M,string:tokens(FA,"/"),Sig),[];	% -R11
    ["p","a name=",FA,"span class=","bold_code",Sig,"/span","/a","br/"|_] ->
      a_line(M,string:tokens(FA,"-"),Sig),[];	% R12-
    ["P","A NAME=",_,"STRONG","CODE"|_] ->	% -R11, broken lines
      Line;
    _ -> 
      case A of
	[] -> [];
	_ -> A++Line
      end
  end.

a_line(M,[F,A],Sig) ->
  try e_bag({fs,M},F),
      e_bag({{as,M},F}, A),
      e_set({{sig,M,F},A}, dehtml(Sig))
  catch _:_ -> ok
  end.

%% --------------------------------------------------------------------------
%% ets-based dictionary
e_set(Key,Val) -> ets:insert(?MODULE,{Key,Val}).

e_get(Key) ->
  case ets:lookup(?MODULE,Key) of
    [{Key,Val}] -> Val;
    [] -> throw(no_data);
    X -> exit({bad_e_get,{table,?MODULE},{key,Key},{value,X}})
  end.

e_bag(Key,Val) ->
  try e_get(Key) of
    L when is_list(L) -> e_set(Key,[Val|L]);
    X                 -> exit({bad_e_bag,{table,?MODULE},{key,Key},{value,X}})
  catch
    no_data -> e_set(Key,[Val])
  end.

%% --------------------------------------------------------------------------
%% the missing fold_file/3 function
fold_file(Fun,Acc0,File) ->
  {ok, FD} = file:open(File, [read]),
  Acc = fold_file_lines(FD,Fun,Acc0),
  file:close(FD),
  Acc.

fold_file_lines(FD,Fun,Acc) ->
  case io:get_line(FD, "") of
    eof -> Acc;
    Line -> fold_file_lines(FD,Fun,Fun(trim_nl(Line),Acc))
  end.

trim_nl(Str) -> lists:reverse(tl(lists:reverse(Str))).
  
%% --------------------------------------------------------------------------
%% Schönfinkelisation
curry(F,Arg) ->
  case erlang:fun_info(F,arity) of
    {_,1} -> fun() -> F(Arg) end;
    {_,2} -> fun(A) -> F(A,Arg) end;
    {_,3} -> fun(A,B) -> F(A,B,Arg) end;
    {_,4} -> fun(A,B,C) -> F(A,B,C,Arg) end
  end.

%% --------------------------------------------------------------------------
io_str(F,A) -> lists:flatten(io_lib:format(F,A)).

dehtml(Str) ->
  element(2,regexp:sub(Str,"&#62;",">")).
