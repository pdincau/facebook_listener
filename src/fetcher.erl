-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/2]).

-define(BASE_URL, <<"https://graph.facebook.com/{objectid}/{field}?access_token={token}&{params}">>).

fetch(AppName, Update) ->
    Entries = entries_in(Update),
    lager:info("Entries in update are: ~p", [Entries]),
    Results = [fetch_entry(AppName, UserId, Fields, Timestamp) || {UserId, Fields, Timestamp} <- Entries],
    lager:info("Results are: ~p", [Results]).

entries_in(Update) ->
    %% TODO: currently only updates with object 'user" are supported
    case Update of
        [{<<"object">>, Object = <<"user">>}, {<<"entry">>, Entries}] ->
            lager:info("Extracted from update: ~p object: ~p and entries: ~p", [Update, Object, Entries]),
            [{UId, Fields, Timestamp} ||  [{<<"uid">>, UId}, {<<"id">>, _Id}, {<<"time">>, Timestamp}, {<<"changed_fields">>, Fields}] <- Entries];
        _ ->
            []
    end.

fetch_entry(AppName, UserId, Fields, _Timestamp) ->
    case access_token(AppName, UserId) of
        {error, _Error} ->
            %% TODO: _Error may be for example be about undefined token or no connection
            ok;
        Token ->
            do_fetch(UserId, Fields, Token)
    end.

do_fetch(UserId, Fields, Token) ->
    %% TODO: this will make a request only for 1st field. Must be rewritten
    Url = url_for(UserId, Fields, Token, <<"limit=1">>),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error])
    end.

access_token(AppName, UserId) ->
    repository:get_access_token(AppName, UserId).

url_for(UserId, Fields, Token, Params) ->
    Url = binary:replace(?BASE_URL, <<"{objectid}">>, UserId),
    Url1 = binary:replace(Url, <<"{field}">>, lists:nth(1, Fields)),
    Url2 = binary:replace(Url1, <<"{token}">>, Token),
    Url3 = binary:replace(Url2, <<"{params}">>, Params),
    binary_to_list(Url3).

-ifdef(TEST).
    -compile(export_all).
-endif.
