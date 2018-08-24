-module(distel_html_doc).

-export([fetch_mods/1, fetch_fas/2]).

%% erlang:get_stacktrace/0 was made obsolete in OTP21
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, StackToken), Class:Reason:StackToken).
-define(GET_STACK(StackToken), StackToken).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

%% internal, exported for debugging
-export([get_apps/0, get_mods/0, get_fas/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

fetch_fas(M, Prefix) when not is_binary(M) -> fetch_fas(to_bin(M), Prefix);
fetch_fas(M, P) when not is_binary(P) -> fetch_fas(M, to_bin(P));
fetch_fas(Mod, Prefix) ->
    start_server(),
    get_fas(Mod),
    ets_get({fas, Mod, Prefix}).

fetch_mods(P) when not is_binary(P) -> fetch_mods(to_bin(P));
fetch_mods(Prefix) ->
    start_server(),
    get_mods(),
    ets_get({mods, Prefix}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation of the information fetching. spider the OTP website,
%% parse the HTML, extract the signatures.
%% the OTP doc is structured like; apps -> mods -> {function, arg}
%% we are not interested in the apps, but we have to use them to get a list
%% of documented modules

base_url() -> "http://erlang.org/doc/apps".


%% get all {function_name, args} for a module
get_fas(Mod) ->
    case ets_get({fas, Mod}) of
        [] ->
            case proplists:get_value(Mod, get_mods(), nil) of
                nil -> [];
                Path -> get_fas(Mod, Path)
            end;
        FAs ->
            FAs
    end.

get_fas(Mod, Path) ->
    URL = join([base_url(), dummy, Path], "/"),
    try trane:wget_sax(URL, fun get_fa/2, {[]}) of
        {FAs} -> ets_put({fas, Mod, FAs})
    catch
        ?EXCEPTION(C, R, S) -> exit({Path, C, R, ?GET_STACK(S)})
    end.

get_fa({tag,"a",Attrs}, State) ->
    case proplists:get_value("name", Attrs, nil) of
        nil ->
            State;
        Name ->
            case re:split(Name, "#|-") of
                [<<>>, Fun, _] -> update_state(Fun, State);
                [Fun, _] -> update_state(Fun, State);
                _ -> State
            end
    end;
get_fa({text, Text}, {Prefix, O}) ->
    case is_prefix(Prefix, Text) of
        true -> {[clean(Text)|O]};
        false -> {Prefix,O}
    end;
get_fa(_, State) ->
    State.

update_state(Fun, {_, O}) -> {Fun, O};
update_state(Fun, {O}) -> {Fun, O}.

clean(Text) ->
    iolist_to_binary(re:replace(Text, "&gt;", ">")).

%% get all {module_name, html_path} for an app

get_mods() ->
    case ets_get(mods) of
        [] ->
            ModPaths = lists:append([get_mods(App) || App <- get_apps()]),
            ets_put({mods, ModPaths});
        ModPaths ->
            ModPaths
    end.

get_mods(App) ->
    URL = join([base_url(), App, "index.html"], "/"),
    trane:wget_sax(URL, fun get_mod/2, []).

get_mod({tag,"a",Attrs}, O) ->
    case proplists:get_value("href", Attrs) of
        undefined -> O;
        X ->
            case re:split(X, "#") of
                [_] -> O;
                [Path, _] -> maybe_add({modname(Path), Path}, O)
            end
    end;
get_mod(_, O) ->
    O.

modname(ModPath) ->
    [<<"html">>, ModName|_] = lists:reverse(re:split(ModPath,"/|\\.")),
    ModName.

maybe_add(H, [H|T]) -> [H|T];
maybe_add(H, List) -> [H|List].

%% get a list of all applications

get_apps() ->
    case ets_get(apps) of
        [] ->
            Apps = trane:wget_sax(base_url(), fun get_app/2, []),
            ets_put({apps, Apps});
        Apps ->
            Apps
    end.

get_app({tag, "a", Attrs}, O) ->
    case proplists:get_value("href", Attrs) of
        X when $a =< hd(X), hd(X) =< $z -> [X|O];
        _ -> O
    end;
get_app(_, O) ->
    O.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some bespoke ets functions. Used to hold our cache of scraped
%% data. We start a server to own the table. ?MODULE is both the ets
%% table name and the registered name of the owner pid.

start_server() ->
    case whereis(?MODULE) of
        undefined ->
            Self = self(),
            Pid = spawn(fun() -> init(Self) end),
            receive {Pid, ok} -> Pid end;
        Pid -> Pid
    end.

init(Parent) ->
    register(?MODULE, self()),
    case ets:file2tab(cache_filename()) of
        {ok, ?MODULE} -> ok;
        {error, _} -> ets:new(?MODULE, [named_table, ordered_set, public])
    end,
    Parent ! {self(), ok},
    receive
        Message -> exit({unrecognized_message, Message})
    end.

cache_filename() ->
    filename:join([os:getenv("HOME"), '.distel', ?MODULE])++".ets".

ets_put({apps, Apps}) ->
    ets:insert(?MODULE, {apps, Apps}),
    persist(),
    Apps;
ets_put({mods, ModPaths}) ->
    [ets:insert(?MODULE, {{mod, Mod}, Path}) || {Mod, Path} <- ModPaths],
    persist(),
    ModPaths;
ets_put({fas, Mod, FAs}) ->
    [ets:insert(?MODULE, {{fa, Mod, FA}}) || FA <- FAs],
    persist(),
    FAs.

%% writes the table to disk. fails silently
persist() ->
    CacheFilename = cache_filename(), 
    case filelib:ensure_dir(CacheFilename) of
        ok -> ets:tab2file(?MODULE, CacheFilename);
        _ -> ok
    end.

ets_get(apps) ->
    case ets:lookup(?MODULE, apps) of
        [] -> [];
        [{apps, Apps}] -> Apps
    end;
ets_get(mods) ->
    [{M, P} || [M, P] <- ets:match(?MODULE, {{mod, '$1'}, '$2'})];
ets_get({mods, Prefix}) ->
    ets_range({mod, Prefix}, 2);
ets_get({fas, Mod}) ->
    lists:append(ets:match(?MODULE, {{fa, Mod, '$1'}}));
ets_get({fas, Mod, Prefix}) ->
    ets_range({fa, Mod, Prefix}, 3).

ets_range(Key, Index) ->
    case ets:lookup(?MODULE, Key) of
        [Obj] -> [element(Index, element(1, Obj))];
        [] -> ets_range(Key, Key, Index, [])
    end.

ets_range(Key, Cur, Index, Acc) ->
    case ets:next(?MODULE, Cur) of
        '$end_of_table' -> Acc;
        New ->
            case is_prefix(element(Index, Key), element(Index, New)) of
                true -> ets_range(Key, New, Index, [element(Index, New)|Acc]);
                false -> Acc
            end
    end.

is_prefix(Pref, Obj) ->
    binary:longest_common_prefix([Pref, Obj]) == size(Pref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

join([], _) -> [];
join([T], _) -> to_list(T);
join([H|T], Sep) -> to_list(H)++Sep++join(T, Sep).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

to_bin(A) when is_atom(A) -> list_to_binary(atom_to_list(A));
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(B) when is_binary(B) -> B.
