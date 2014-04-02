-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/2]).

-define(BASE_URL, "https://graph.facebook.com/815447318/friends?fields=name&access_token=").

fetch(AppName, Update) ->
    case application_credentials(AppName) of
        {AppId, AppSecret} ->
            Entries = entries_in(Update),
            lager:info("Entries in update are: ~p", [Entries]),
            Address = lists:concat([?BASE_URL, access_token(AppId, AppSecret)]),
            do_fetch(Address);
        _ ->
            lager:warning("Undefined application name: ~p", [AppName])
    end.

application_credentials(_AppName) ->
    %% For now this is super stubbed
    {"app_id", "app_secret"}.

access_token(AppId, AppSecret) ->
    lists:concat([AppId, "|", AppSecret]).

do_fetch(Address) ->
    case httpc:request(Address) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
             %% For now I just log the list of my friends
            lager:info("My Facebook friends are: ~p", [Body]);
        Error ->
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error])
    end.

entries_in(Update) ->
    %% For now I just match 'user" object
    [{<<"object">>, Object = <<"user">>}, {<<"entry">>, Entries}] = Update,

    lager:info("Extracted from update: ~p object: ~p and entries: ~p", [Update, Object, Entries]),

    [{UId, Fields, Timestamp} || [{<<"uid">>, UId}, {<<"changed_fields">>, Fields}, {<<"time">>, Timestamp}] <- Entries].
