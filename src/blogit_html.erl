-module(blogit_html).

-export([html/1, html/2, body/1, body/2, head/1]).
-export([title/1, script/1, script/2, link/1, meta/1]).
-export([htmldiv/1, htmldiv/2]).
-export([h1/1, h1/2, h2/1, h2/2]).
-export([p/1, p/2]).
-export([hr/0]).

-type option() :: {binary() | string(), binary(), string()}.

-spec html(iolist()) -> iolist().
html(Parts) -> html(Parts, []).

-spec html(iolist(), [option()]) -> iolist().
html(Parts, Options) ->
    build_tag(<<"html">>, Parts, Options).

-spec body(iolist()) -> iolist().
body(Parts) -> body(Parts, []).

-spec body(iolist(), [option()]) -> iolist().
body(Parts, Options) ->
    build_tag(<<"body">>, Parts, Options).

-spec head(iolist()) -> iolist().
head(Parts) ->
    build_tag(<<"head">>, Parts).

-spec script(iolist()) -> iolist().
script(Parts) ->
    script(Parts, []).

-spec script(iolist(), [option()]) -> iolist().
script(Parts, Options) ->
    build_tag(<<"script">>, Parts, Options).

-spec link(iolist()) -> iolist().
link(Options) ->
    build_closed_tag(<<"link">>, Options).

-spec title(iolist()) -> iolist().
title(Parts) ->
    build_tag(<<"title">>, Parts).

-spec meta(iolist()) -> iolist().
meta(Options) ->
    build_closed_tag(<<"meta">>, Options).

-spec htmldiv(iolist()) -> iolist().
htmldiv(Parts) -> htmldiv(Parts, []).

-spec htmldiv(iolist(), [option()]) -> iolist().
htmldiv(Parts, Options) ->
    build_tag(<<"div">>, Parts, Options).

-spec h1(iolist()) -> iolist().
h1(Parts) -> h1(Parts, []).

-spec h1(iolist(), [option()]) -> iolist().
h1(Parts, Options) ->
    build_tag(<<"h1">>, Parts, Options).

-spec h2(iolist()) -> iolist().
h2(Parts) -> h2(Parts, []).

-spec h2(iolist(), [option()]) -> iolist().
h2(Parts, Options) ->
    build_tag(<<"h2">>, Parts, Options).

-spec p(iolist()) -> iolist().
p(Parts) -> p(Parts, []).

-spec p(iolist(), [option()]) -> iolist().
p(Parts, Options) ->
    build_tag(<<"p">>, Parts, Options).

-spec hr() -> iolist().
hr() -> <<"<hr />">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option_join(option()) -> iolist().
option_join({K, V}) ->
    [<<" ">>, K, <<"=\"">>, V, <<"\"">>].

build_tag(Tag,Parts) ->
    [<<"<">>, Tag, <<">">>, Parts, <<"</">>, Tag, <<">">>].

build_tag(Tag, Parts, Options) ->
    [<<"<">>, Tag, lists:map(fun option_join/1, Options), <<">">>,
     Parts, <<"</">>, Tag, <<">">>].

build_closed_tag(Tag, Options) ->
    [<<"<">>, Tag, lists:map(fun option_join/1, Options), <<" />">>].
