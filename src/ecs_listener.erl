-module(ecs_listener).

-behaviour(eunit_listener).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
terminate/2]).

-record(state, {verbose = false,
		indent = 0,
		pid = "."
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options),
		pid = proplists:get_value(pid, Options)},
    put(no_tty, true),
    receive
	{start, _Reference} ->
	    St
    end.

terminate({ok, Data}, St) ->
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    send_back(St, Data),
    if Fail =:= 0, Skip =:= 0, Cancel =:= 0 ->
	    sync_end(ok);
       true ->
	    sync_end(error)
    end;
terminate({error, _Reason}, _St) ->
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

handle_begin(_L, _Data, St) ->
    St.

handle_end(_L, Data, St) ->
    send_back(St, Data),
    St.

handle_cancel(_L, Data, St) ->
    send_back(St, Data),
    St.

send_back(#state{pid = Pid} = _St, Msg) ->
    Pid ! Msg.
