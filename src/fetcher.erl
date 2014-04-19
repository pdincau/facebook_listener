-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([handle/2, handle/3]).

-define(BASE_URL, <<"https://graph.facebook.com/{objectid}/{field}?access_token={token}&{params}">>).

handle(AppName, Update) ->
    handle(AppName, Update, fun push/1).

handle(AppName, Update, Fun) ->
    Entries = parser:entries_in(Update),
    lager:info("Entries in update are: ~p", [Entries]),
    [fetch_entry(AppName, Entry, Fun) || Entry <- Entries].

fetch_entry(AppName, Entry, Fun) ->
    {UId, ChangedFields, _Time} = parser:values_in(Entry),
    case access_token(AppName, UId) of
        {error, Error} ->
            {error, Error};
        {token, Token} ->
            Results = [do_fetch(UId, Field, Token) || Field <- ChangedFields],
            [Fun(Result) || Result <- Results]
    end.

do_fetch(UserId, Field, Token) ->
    Url = url_for(UserId, Field, Token, <<"limit=1">>),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            %% TODO: Identify better error
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error]),
            {error, fetch}
    end.

access_token(AppName, UserId) ->
    gen_server:call(repository, {access_token, {AppName, UserId}}).

url_for(UserId, Field, Token, Params) ->
    Url = binary:replace(?BASE_URL, <<"{objectid}">>, UserId),
    Url1 = binary:replace(Url, <<"{field}">>, Field),
    Url2 = binary:replace(Url1, <<"{token}">>, Token),
    Url3 = binary:replace(Url2, <<"{params}">>, Params),
    binary_to_list(Url3).

push({error, fetch}) ->
    ok;

push(Msg) ->
    gen_server:cast(queue, {push, Msg}).
