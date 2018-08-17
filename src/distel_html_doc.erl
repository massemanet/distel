-module(distel_html_doc).

-export([fetch_mods/0, fetch_fas/1]).

%% internal
-export([get_apps/0, get_mods/1, get_fas/1]).

%% {tag,"a",
%%       [{"class","title_link"},
%%        {"name","compile-1"},
%%        {"href","#compile-1"}]},
%%  {text,<<"compile(Asn1module) -&gt; ok | {error, Reason}">>},

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

fetch_fas(Mod) ->
    get_fas(proplists:get_value(Mod, fetch_mods())).

fetch_mods() ->
    lists:flatten([get_mods(App) || App <- get_apps()]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

base_url() -> "http://erlang.org/doc/apps".

get_apps() ->
    trane:wget_sax(base_url(), fun get_app/2, []).

get_app({tag, "a", Attrs}, O) ->
    case proplists:get_value("href", Attrs) of
        X when $a =< hd(X), hd(X) =< $z ->
            [X|O];
        _ ->
            O
    end;
get_app(_, O) ->
    O.

get_mods(App) ->
    URL = join([base_url(), App, "index.html"]),
    trane:wget_sax(URL, fun get_mod/2, []).

get_mod({tag,"a",Attrs}, O) ->
    case proplists:get_value("href", Attrs) of
        undefined -> O;
        X ->
            case re:split(X, "#") of
                [_] -> O;
                [M,_F] ->
                    case lists:member(M, O) of
                        true -> O;
                        false -> [{modname(M), M}|O]
                    end
            end
    end;
get_mod(_, O) ->
    O.

modname(ModPath) ->
    [<<"html">>,ModName|_] = lists:reverse(re:split(ModPath,"/|\\.")),
    ModName.

get_fas(Path) ->
    URL = join([base_url(), dummy, Path]),
    try trane:wget_sax(URL, fun get_fa/2, {null, []}) of
        {null, FAs} -> FAs
    catch
        _:R -> exit({Path, R})
    end.

get_fa({tag,"a",Attrs}, {null, O}) ->
    case lists:sort(proplists:get_keys(Attrs)) of
        ["class", "href", "name"] ->
            case re:split(proplists:get_value("href", Attrs), "#|-") of
                [<<>>, Fun, _] -> {Fun, O};
                _ -> {null, O}
            end;
        _ ->  {null, O}
    end;
get_fa(_, {null, O}) ->
    {null, O};
get_fa({text, Text}, {Prefix, O}) ->
    case binary:longest_common_prefix([Text, Prefix]) == size(Prefix) of
        true -> {null, [clean(Text)|O]};
        false -> {null, O}
    end;
get_fa(Entity, {State, O}) ->
    throw({fail, Entity, State, O}).

clean(Text) ->
    iolist_to_binary(re:replace(Text, "&gt;", ">")).

join([]) -> [];
join([T]) -> to_list(T);
join([H|T]) -> to_list(H)++"/"++join(T).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.
