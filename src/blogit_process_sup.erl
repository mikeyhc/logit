-module(blogit_process_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
                 intensity => 3,
                 period => 30},
    ChildSpecs = [#{id => blogit_server,
                    start => {blogit_server, start, []}},
                  #{id => blogit_inserter_scheduler,
                    start => {blogit_scheduler, start_link,
                              [[{{blogit_inserter, run, ["test_repos/pull"]},
                                 120000}]]}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
