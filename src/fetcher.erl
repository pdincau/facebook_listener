-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/1]).

-define(BASE_URL, "https://graph.facebook.com/815447318/friends?fields=name&access_token=").

fetch(_AppName) ->
    Address = lists:concat([?BASE_URL, access_token()]),

    case httpc:request(Address) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            %% For now I just log the list of my friends
            lager:info("My Facebook friends are: ~p", [Body]);
        Error ->
            lager:warning("Could fetch from url: ~p~n. Response was: ~p", [Error])
    end.

access_token() ->
    lists:concat(["app_id", "|", "your_app_secret"]).
