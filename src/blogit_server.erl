-module(blogit_server).

-export([start/0]).

start() ->
    Routes = [{"/", index_handler, []},
              {"/scripts/[...]", cowboy_static,
               {priv_dir, blogit, "static/scripts"}},
              {"/css/[...]", cowboy_static, {priv_dir, blogit, "static/css"}}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(blogit_ui,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    blogit_server_sup:start_link().
