-module(blogit_blog).

-export([read/1, serialize/1, deserialize/1]).

-type blog() :: binary().

-spec read(file:name_all()) -> blog().
read(File) ->
    {ok, Contents} = file:read_file(File),
    Contents.

-spec serialize(blog()) -> binary().
serialize(Blog) ->
    Blog.

-spec deserialize(binary()) -> blog().
deserialize(Blog) ->
    Blog.
