-module(blogit_git).

-export([open_repo/1, first_commit/1, get_head/1, ls_files/2, diff_files/3,
         dir/1]).

-record(blogit_git_repo, {path :: string()}).
-type blogit_git_repo() :: #blogit_git_repo{}.

-spec open_repo(string()) -> blogit_git_repo().
open_repo(Path) ->
    #blogit_git_repo{path = Path}.

-spec first_commit(blogit_git_repo()) -> string().
first_commit(Git) ->
    string:trim(git_exec(Git, "rev-list --max-parents=0 HEAD")).

-spec get_head(blogit_git_repo()) -> binary().
get_head(Git) ->
    binary:list_to_bin(string:trim(git_exec(Git, "rev-list -n1 HEAD"))).

-spec ls_files(blogit_git_repo(), string()) -> [string()].
ls_files(Git, Commit) ->
    Data = git_exec(Git, "ls-tree -r ~s", [Commit]),
    Lines = string:split(Data, "\n", all),
    F = fun(S) ->
                [_|File] = string:split(S, "\t"),
                File
        end,
    lists:flatmap(F, Lines).

-spec diff_files(blogit_git_repo(), string(), string()) -> [string()].
diff_files(Git, Commit1, Commit2) ->
    Data = git_exec(Git, "diff --name-only ~s ~s", [Commit1, Commit2]),
    string:split(string:trim(Data), "\n").

-spec dir(blogit_git_repo()) -> string().
dir(#blogit_git_repo{path=Path}) -> Path.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

git_exec(Git, Command) -> git_exec(Git, Command, []).

git_exec(#blogit_git_repo{path=Path}, Command, Args) ->
    {0, Ret} = exec("git --git-dir=" ++ Path ++ "/.git " ++ Command, Args),
    Ret.

exec(Command, Args) ->
    Cmd = lists:flatten(io_lib:format(Command, Args)),
    logger:info("running command \"~s\"", [Cmd]),
    Port = open_port({spawn, Cmd}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
            {Port, closed} -> true
        end,
        receive
            {'EXIT',  Port,  _} -> ok
        after 1 -> ok % force context switch
        end,
        ExitCode = receive
            {Port, {exit_status, Code}} ->Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
