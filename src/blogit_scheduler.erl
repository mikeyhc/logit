-module(blogit_scheduler).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(ProcSpec) ->
    gen_server:start_link(?MODULE, ProcSpec, []).

init(ProcSpec) ->
    gen_server:cast(self(), start_procs),
    {ok, ProcSpec}.

handle_call(_Message, _From, _State) ->
    throw(not_implemented).

handle_cast(start_procs, State) ->
    F = fun({{M, F, A}, Time}) ->
                {ok, Ref} = timer:apply_interval(Time, M, F, A),
                {{M, F, A}, Time, Ref}
        end,
    {noreply, lists:map(F, State)}.
