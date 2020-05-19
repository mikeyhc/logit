-module(blogit_metadata).

-export([read/1, serialize/1, deserialize/1]).

-type metadata() :: maps:map().

-spec read(file:name_all()) -> metadata().
read(File) ->
    {ok, Contents} = file:read_file(File),
    jsone:decode(Contents).

-spec serialize(metadata()) -> binary().
serialize(Metadata) ->
    jsone:encode(Metadata).

-spec deserialize(binary()) -> metadata().
deserialize(Metadata) ->
    jsone:decode(Metadata).
