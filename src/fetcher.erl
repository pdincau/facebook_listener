-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/2]).

-define(BASE_URL, <<"https://graph.facebook.com/{objectid}/{field}?access_token={token}&{params}">>).

fetch(AppName, Update) ->
    Entries = entries_in(Update),
    lager:info("Entries in update are: ~p", [Entries]),
    Results = [do_fetch(AppName, UserId, Fields, Timestamp) || {UserId, Fields, Timestamp} <- Entries],	
    lager:info("Results are: ~p", [Results]).

access_token(_AppName, _UserId) ->
    %% TODO: Implement token recovery here.
    <<"securitytoken">>.

do_fetch(AppName, UserId, Fields, _Timestamp) ->
    Url1 = binary:replace(?BASE_URL, <<"{objectid}">>, UserId),
    %% TODO: this will make a request only for 1st field. Must be rewritten
    Url2 = binary:replace(Url1, <<"{field}">>, lists:nth(1, Fields)),
    Url3 = binary:replace(Url2, <<"{token}">>, access_token(AppName, UserId)),
    Url4 = binary:replace(Url3, <<"{params}">>, <<"limit=1">>),
    case httpc:request(binary_to_list(Url4)) of
        {ok, {{_, 200, _}, _Headers, Body}} -> 
            Body;
        Error ->
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error])
    end.

entries_in(Update) ->
    %% TODO: currently only updates with object 'user" are supported
    case Update of
        [{<<"object">>, Object = <<"user">>}, {<<"entry">>, Entries}] ->
            lager:info("Extracted from update: ~p object: ~p and entries: ~p", [Update, Object, Entries]),
            [{UId, Fields, Timestamp} ||  [{<<"uid">>, UId}, {<<"id">>, _Id}, {<<"time">>, Timestamp}, {<<"changed_fields">>, Fields}] <- Entries];
        _ ->
            []
    end.

-ifdef(TEST).
    -compile(export_all).
-endif.
