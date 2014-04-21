-module(subscriber).

-export([subscriptions/0]).

-define(BASE_URL, <<"https://graph.facebook.com/{appid}/subscriptions?access_token={appid}|{secret}">>).

subscriptions() ->
    {ok, AppId} = application:get_env(facebook_listener, app_id),
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    Url = url_for(AppId, AppSecret),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error]),
            {error, fetch}
    end.


url_for(AppId, AppSecret) ->
    Url = binary:replace(?BASE_URL, <<"{appid}">>, AppId, [global]),
    Url1 = binary:replace(Url, <<"{secret}">>, AppSecret),
    binary_to_list(Url1).
