-module(subscriber).

-export([subscriptions/0, subscribe/3]).

-define(BASE_URL, <<"https://graph.facebook.com/{appid}/subscriptions?access_token={appid}|{secret}">>).
-define(BASE_BODY, <<"object={object}&callback_url={callback_url}&fields={fields}&verify_token=token">>).

subscriptions() ->
    {ok, AppId} = application:get_env(facebook_listener, app_id),
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    Url = url_for(AppId, AppSecret),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            lager:warning("Couldn't fetch subscriptions. Response was: ~p", [Error]),
            {error, Error}
    end.

subscribe(Object, CallbackUrl, Fields) ->
    {ok, AppId} = application:get_env(facebook_listener, app_id),
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    Url = url_for(AppId, AppSecret),

    Body = body_for(Object, CallbackUrl, Fields),

    case httpc:request(post, {Url, [], "", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            lager:warning("Couldn't update subscriptions. Response was: ~p", [Error]),
            {error, Error}
    end.

url_for(AppId, AppSecret) ->
    Url = binary:replace(?BASE_URL, <<"{appid}">>, AppId, [global]),
    Url1 = binary:replace(Url, <<"{secret}">>, AppSecret),
    binary_to_list(Url1).

body_for(Object, CallbackUrl, Fields) ->
    Body = binary:replace(?BASE_BODY, <<"{object}">>, Object),
    Body1 = binary:replace(Body, <<"{callback_url}">>, CallbackUrl),
    Body2 = binary:replace(Body1, <<"{fields}">>, Fields),
    binary_to_list(Body2).
