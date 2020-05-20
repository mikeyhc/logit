-module(blogit_db).
-behaviour(gen_server).

-export([start_link/1, get_head/1, has_entry/2, insert_metadata/3,
         update_metadata/3, insert_blog/3, store_head/2, get_latest_entries/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {address :: string(),
                port :: pos_integer(),
                mref :: reference() | undefined,
                connection}).
-define(TIMESTAMP_FORMAT, "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(ConnParams) ->
    gen_server:start_link(?MODULE, [ConnParams], []).

-spec get_head(pid()) -> {ok, binary()} | {error, not_found}.
get_head(Pid) ->
    gen_server:call(Pid, get_head).

-spec has_entry(pid(), string()) -> boolean().
has_entry(Pid, Entry) ->
    gen_server:call(Pid, {has_entry, Entry}).

-spec insert_metadata(pid(), string(), blogit_metadata:metadata()) -> any().
insert_metadata(Pid, ID, Data) ->
    gen_server:call(Pid, {insert_metadata, ID, Data}).

-spec update_metadata(pid(), string(), blogit_metadata:metadata()) -> any().
update_metadata(Pid, ID, Data) ->
    gen_server:call(Pid, {update_metadata, ID, Data}).

-spec insert_blog(pid(), string(), blogit_blog:blog()) -> any().
insert_blog(Pid, ID, Data) ->
    gen_server:call(Pid, {insert_blog, ID, Data}).

-spec store_head(pid(), binary()) -> any().
store_head(Pid, Data) ->
    gen_server:call(Pid, {store_head, Data}).

-spec get_latest_entries(pid(), non_neg_integer()) -> [any()].
get_latest_entries(Pid, Count) ->
    gen_server:call(Pid, {get_latest_entries, Count}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ConnParams]) ->
    gen_server:cast(self(), connect),
    Address = maps:get(address, ConnParams),
    Port = maps:get(port, ConnParams, 5984),
    {ok, #state{address=Address, port=Port}}.

handle_call(get_head, _From, S=#state{connection=Conn}) ->
    {reply, get_head_(Conn), S};
handle_call({has_entry, Entry}, _From, S=#state{connection=Conn}) ->
    {reply, has_entry_(Conn, Entry), S};
handle_call({insert_metadata, ID, Data}, _From, S=#state{connection=Conn}) ->
    NewData = maps:put(<<"timestamp">>, timestamp(), Data),
    {reply, insert_metadata_(Conn, ID, NewData), S};
handle_call({update_metadata, ID, Data}, _From, S=#state{connection=Conn}) ->
    {reply, update_metadata_(Conn, ID, Data), S};
handle_call({insert_blog, ID, Data}, _From, S=#state{connection=Conn}) ->
    {reply, insert_blog_(Conn, ID, Data), S};
handle_call({store_head, Data}, _From, S=#state{connection=Conn}) ->
    {reply, store_head_(Conn, Data), S};
handle_call({get_latest_entries, Count}, _From, S=#state{connection=Conn}) ->
    {reply, get_latest_entries_(Conn, Count), S}.

handle_cast(connect, S=#state{address=Address, port=Port}) ->
    {ok, ConnPid} = gun:open(Address, Port),
    MRef = monitor(process, ConnPid),
    {ok, _Protocol} = gun:await_up(ConnPid),
    {noreply, S#state{connection=ConnPid, mref=MRef}}.

handle_info({'DOWN', MRef, process, ConnPid, Reason},
            #state{connection=ConnPid, mref=MRef}) ->
    logger:error("connection to couchdb closed: ~p", [Reason]),
    exit(Reason);
handle_info({gun_down, ConnPid, http, closed, []},
            State=#state{connection=ConnPid}) ->
    logger:warning("connection to couchdb closed"),
    {noreply, State};
handle_info({gun_up, ConnPid, http}, State=#state{connection=ConnPid}) ->
    logger:warning("gun re-upped"),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_head_(Conn) ->
    {Status, Data} = auth_get(Conn, "/metadata/head"),
    case Status of
        404 -> {error, not_found};
        200 ->
            #{<<"head">> := Head} = jsone:decode(Data),
            {ok, Head};
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            exit(Status)
    end.

has_entry_(Conn, Entry) ->
    Status = auth_head(Conn, "/entry/" ++ Entry),
    case Status of
        404 -> false;
        200 -> true;
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            exit(Status)
    end.

insert_metadata_(Conn, ID, Data) ->
    Serialized = blogit_metadata:serialize(Data),
    {Status, Body} = auth_put(Conn, "/entry/" ++ ID, Serialized),
    case is_success(Status) of
        true -> ok;
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            logger:error(Body),
            exit(Status)
    end.

update_metadata_(Conn, ID, Data) ->
    {ok, Entry} = get_entry_(Conn, ID),
    NewEntry = maps:merge(Entry, Data),
    insert_metadata_(Conn, ID, NewEntry).

get_entry_(Conn, ID) ->
    {Status, Body} = auth_get(Conn, "/entry/" ++ ID),
    case Status of
        404 -> {error, not_found};
        200 -> {ok, blogit_metadata:deserialize(Body)};
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            exit(Status)
    end.

insert_blog_(Conn, ID, Data) ->
    {ok, Entry} = get_entry_(Conn, ID),
    NewEntry = maps:put(<<"blog">>, blogit_blog:serialize(Data), Entry),
    insert_metadata_(Conn, ID, NewEntry).

get_full_head_(Conn) ->
    {Status, Data} = auth_get(Conn, "/metadata/head"),
    case Status of
        404 -> {error, not_found};
        200 -> {ok, jsone:decode(Data)};
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            exit(Status)
    end.

store_head_(Conn, Head) ->
    NewData = case get_full_head_(Conn) of
                  {ok, Data0} ->
                      #{<<"head">> := OldHead,
                        <<"history">> := History} = Data0,
                      Data1 = maps:put(<<"head">>, Head, Data0),
                      maps:put(<<"history">>, [OldHead|History], Data1);
                  {error, not_found} ->
                      #{<<"head">> => Head, <<"history">> => []}
              end,
    Serialized = jsone:encode(NewData),
    {Status, Body} = auth_put(Conn, "/metadata/head", Serialized),
    case is_success(Status) of
        true -> ok;
        _ ->
            logger:error("invalid status from couchdb: ~p", [Status]),
            logger:error(Body),
            exit(Status)
    end.

get_latest_entries_(Conn, Count) ->
    Query = lists:flatten(io_lib:format("/entry/_design/blogs/_view/blog-index"
                                        "?descending=true"
                                        "&limit=~w"
                                        "&include_docs=true",
                                        [Count])),
    {200, JsonData} = auth_get(Conn, Query),
    #{<<"rows">> := Data} = jsone:decode(JsonData),
    F = fun(#{<<"doc">> := Doc}) ->
                #{<<"title">> := Title, <<"blog">> := Blog,
                  <<"timestamp">> := Timestamp} = Doc,
                Author = maps:get(<<"author">>, Doc, <<"anonymous">>),
                #{title => Title, blog => Blog, timestamp => Timestamp,
                  author => Author}
        end,
    lists:map(F, Data).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Stmp = io_lib:format(?TIMESTAMP_FORMAT, [Year, Month, Day, Hour, Min, Sec]),
    binary:list_to_bin(lists:flatten(Stmp)).

auth_get(ConnPid, Path) ->
    StreamRef = gun:get(ConnPid, Path,
                       [{<<"Authorization">>,
                         <<"Basic YWRtaW46cGFzc3dvcmQ=">>},
                        {<<"Accept">>, <<"application/json">>}]),
    Ret = receive
              {gun_response, ConnPid, StreamRef, fin, Status, _Headers} ->
                  {Status,  no_data};
              {gun_response, ConnPid, StreamRef, nofin, Status, _Headers} ->
                  {Status, receive_data(ConnPid, StreamRef, [])}
          after 1000 ->
                    exit(timeout)
          end,
    Ret.

auth_head(ConnPid, Path) ->
    StreamRef = gun:head(ConnPid, Path,
                       [{<<"Authorization">>,
                         <<"Basic YWRtaW46cGFzc3dvcmQ=">>},
                        {<<"Accept">>, <<"application/json">>}]),
    Ret = receive
              {gun_response, ConnPid, StreamRef, fin, Status, _Headers} ->
                  Status
          after 1000 ->
                    exit(timeout)
          end,
    Ret.

auth_put(ConnPid, Path, Data) ->
    StreamRef = gun:put(ConnPid, Path,
                       [{<<"Authorization">>,
                         <<"Basic YWRtaW46cGFzc3dvcmQ=">>},
                        {<<"Accept">>, <<"application/json">>}]),
    gun:data(ConnPid, StreamRef, fin, Data),
    Ret = receive
              {gun_response, ConnPid, StreamRef, fin, Status, _Headers} ->
                  {Status,  no_data};
              {gun_response, ConnPid, StreamRef, nofin, Status, _Headers} ->
                  {Status, receive_data(ConnPid, StreamRef, [])}
          after 1000 ->
                    exit(timeout)
          end,
    Ret.

receive_data(ConnPid, StreamRef, Acc) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_data(ConnPid, StreamRef, [Data|Acc]);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            lists:foldl(fun bin_join/2, <<>>, lists:reverse([Data|Acc]))
    after 1000 ->
              exit(timeout)
    end.

is_success(Code) -> Code >= 200 andalso Code < 300.

bin_join(H, T) -> <<H/binary, T/binary>>.
