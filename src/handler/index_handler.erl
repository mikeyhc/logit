-module(index_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    DB = blogit_sup:get_db(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           page_build(
                             "Blog",
                             content_div(
                               [page_title("Blog"),
                                blog_list(DB)
                               ])),
                           Req0),
    {ok, Req, State}.

content_div(Parts) ->
    blogit_html:htmldiv(Parts, [{"class", "content"}]).

page_title(Title) ->
    blogit_html:htmldiv(blogit_html:h1(Title, [{"class", "page-title"}]),
                        [{"class", "title-bar"}]).

page_build(Title, Body) ->
    [<<"<!doctype html>\n\n">>,
     blogit_html:html([
       blogit_html:head(
         [blogit_html:title(Title),
          blogit_html:meta([{"charset", "UTF-8"}]),
          blogit_html:link([{"rel", "stylesheet"},
                            {"href", "css/workshop.css"}]),
          blogit_html:script([], [{"src", "scripts/workshop.js"}])
         ]),
       blogit_html:body(Body)
       ])].

blog_list(DB) ->
    Entries = blogit_db:get_latest_entries(DB, 10),
    lists:map(fun to_html_blog/1, Entries).

to_html_blog(#{title := Title, author := Author, blog := Blog,
               timestamp := Time}) ->
    [blogit_html:h2(Title),
     blogit_html:p([Author, "&nbsp;-&nbsp;", Time], [{"class", "author"}]),
     Blog,
     blogit_html:hr()
    ].


