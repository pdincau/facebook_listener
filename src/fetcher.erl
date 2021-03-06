-module(fetcher).

-export([handle/2, handle/3]).

-define(BASE_URL, <<"https://graph.facebook.com/{objectid}/{field}?access_token={token}&{params}">>).
-define(PARAMS, <<"since={since}&until={until}">>).

handle(AppName, Update) ->
    handle(AppName, Update, fun push/1).

handle(AppName, Update, Fun) ->
    Entries = parser:entries_in(Update),
    error_logger:info_report(["Entries in update", {entries, Entries}]),
    [fetch_entry(AppName, Entry, Fun) || Entry <- Entries].

fetch_entry(AppName, Entry, Fun) ->
    {UId, ChangedFields, Time} = parser:values_in(Entry),
    case access_token(AppName, UId) of
        {error, Error} ->
            {error, Error};
        {token, Token} ->
            SocialActivities = [do_fetch(UId, Field, Token) || Field <- ChangedFields],
            update_timestamp(UId, Time),
            [Fun(Activity) || Activity <- SocialActivities]
    end.

do_fetch(UserId, Field, Token) ->
    Url = url_for(UserId, Field, Token),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            %% TODO: Identify better error
            error_logger:info_report(["Couldn't fetch update. Response was:", {response, Error}]),
            {error, fetch}
    end.

url_for(UserId, <<"likes">>, Token) ->
    url_for(UserId, <<"likes">>, Token, <<"">>);

url_for(UserId, Field, Token) ->
    Since = last_timestamp(UserId),
    Params = params_for(Since),
    url_for(UserId, Field, Token, Params).

url_for(UserId, Field, Token, Params) ->
    Url = binary:replace(?BASE_URL, <<"{objectid}">>, UserId),
    Url1 = binary:replace(Url, <<"{field}">>, Field),
    Url2 = binary:replace(Url1, <<"{token}">>, Token),
    Url3 = binary:replace(Url2, <<"{params}">>, Params),
    binary_to_list(Url3).

params_for(Since) ->
    Params = binary:replace(?PARAMS, <<"{since}">>, Since),
    binary:replace(Params, <<"{until}">>, <<"">>).

access_token(AppName, UserId) ->
    gen_server:call(repository, {access_token, {AppName, UserId}}).

last_timestamp(UserId) ->
    gen_server:call(repository, {last_timestamp, UserId}).

update_timestamp(UserId, Timestamp) ->
    BinTimestamp = integer_to_binary(Timestamp),
    gen_server:cast(repository, {new_timestamp, UserId, BinTimestamp}).

push({error, fetch}) ->
    ok;

push(Activity) ->
    gen_server:cast(queue, {push, Activity}).
