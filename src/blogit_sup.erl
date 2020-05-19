%%%-------------------------------------------------------------------
%% @doc blogit top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blogit_sup).

-behaviour(supervisor).

-export([start_link/0, get_db/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_db() ->
    Children = supervisor:which_children(?SERVER),
    {_Id, Pid, _Type, _Modes} = lists:keyfind(blogit_db, 1, Children),
    Pid.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => blogit_db,
                    start => {blogit_db, start_link, [#{address => "localhost"}]}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
