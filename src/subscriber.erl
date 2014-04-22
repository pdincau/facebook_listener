-module(subscriber).

-export([subscriptions/0, subscribe/3, unsubscribe/1]).

-define(BASE_URL, <<"https://graph.facebook.com/{appid}/subscriptions?access_token={appid}|{secret}">>).
-define(BASE_BODY, <<"object={object}&callback_url={callback_url}&fields={fields}&verify_token={token}">>).
-define(VERIFICATION_TOKEN, <<"token">>).

subscriptions() ->
    {ok, AppId} = application:get_env(facebook_listener, app_id),
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    Url = url_for(AppId, AppSecret),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            io:format("Couldn't update subscriptions. Response was: ~p~n", [Error]),
            {error, Error}
    end.

subscribe(Object, CallbackUrl, Fields) ->
    {ok, Token} = application:get_env(facebook_listener, verification_token),
    RequestBody = body_for(Object, CallbackUrl, Fields, Token),

    do_request(post, RequestBody).

unsubscribe(Objects) ->
    do_request(delete, Objects).

do_request(Verb, Body) ->
    {ok, AppId} = application:get_env(facebook_listener, app_id),
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    Url = url_for(AppId, AppSecret),

    case httpc:request(Verb, {Url, [], "", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            ResponseBody;
        Error ->
            io:format("Couldn't complete. Response was: ~p~n", [Error]),
            {error, Error}
    end.

url_for(AppId, AppSecret) ->
    Url = binary:replace(?BASE_URL, <<"{appid}">>, AppId, [global]),
    Url1 = binary:replace(Url, <<"{secret}">>, AppSecret),
    binary_to_list(Url1).

body_for(Object, CallbackUrl, Fields, Token) ->
    Body = binary:replace(?BASE_BODY, <<"{object}">>, Object),
    Body1 = binary:replace(Body, <<"{callback_url}">>, CallbackUrl),
    Body2 = binary:replace(Body1, <<"{fields}">>, Fields),
    Body3 = binary:replace(Body2, <<"{token}">>, Token),
    binary_to_list(Body3).
