-module(blogit_inserter).

-export([run/1]).

is_metadata(File) ->
    lists:suffix("metadata.json", File).

is_blog(File) ->
    lists:suffix("blog.html", File).

run(Path) ->
    logger:info("running inserter with path ~s", [Path]),
    Repo = blogit_git:open_repo(Path),
    DB = blogit_sup:get_db(),
    blogit_git:pull(Repo),
    OldHead = case blogit_db:get_head(DB) of
                  {ok, Sha} -> Sha;
                  {error, not_found} -> not_found
              end,
    logger:info("previous head was ~s", [OldHead]),
    Head = blogit_git:get_head(Repo),
    logger:info("current head is ~s", [Head]),
    if OldHead =:= Head ->
           logger:info("no changes, exiting");
       true -> process_files(Repo, DB, OldHead, Head)
    end.

process_files(Repo, DB, OldHead, Head) ->
    Files = case OldHead of
                not_found -> blogit_git:ls_files(Repo, Head);
                _ -> blogit_git:diff_files(Repo, OldHead, Head)
            end,
    Root = blogit_git:dir(Repo),
    logger:info("found changed files ~p", [Files]),
    Metadata = lists:filter(fun is_metadata/1, Files),
    logger:info("found ~p metadata updates", [length(Metadata)]),
    Blogs = lists:filter(fun is_blog/1, Files),
    logger:info("found ~p blog updates", [length(Blogs)]),
    lists:foreach(fun(File) -> write_metadata(DB, Root, File) end, Metadata),
    lists:foreach(fun(File) -> write_blog(DB, Root, File) end, Blogs),
    blogit_db:store_head(DB, Head).

write_metadata(DB, Root, File) ->
    ID = get_id(File),
    Data = blogit_metadata:read(Root ++ "/" ++ File),
    case blogit_db:has_entry(DB, ID) of
        true -> blogit_db:update_metadata(DB, ID, Data);
        % TODO get creation time from file
        false -> blogit_db:insert_metadata(DB, ID, Data)
    end.

write_blog(DB, Root, File) ->
    ID = get_id(File),
    case blogit_db:has_entry(DB, ID) of
        true ->
            Data = blogit_blog:read(Root ++ "/" ++ File),
            blogit_db:insert_blog(DB, ID, Data);
        false ->
            logger:warning("missing metadata for ~s, cannot add blog", [ID])
    end.

get_id(Path) ->
    lists:takewhile(fun(X) -> X =/= $\/ end, Path).
