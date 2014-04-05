%% Feel free to use, reuse and abuse the code in this file.

%% @doc Facebook Real-time updates handler.
-module(handler).

-compile([{parse_transform, lager_transform}]).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HUB_MODE, <<"subscribe">>).
-define(VERIFICATION_TOKEN, <<"token">>).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reply(Method, Req2),
    {ok, Req3, State}.

reply(<<"GET">>, Req) ->
    {Mode, Req2} = cowboy_req:qs_val(<<"hub.mode">>, Req),
    {VerifyToken, Req3} = cowboy_req:qs_val(<<"hub.verify_token">>, Req2),
    {Challenge, Req4} = cowboy_req:qs_val(<<"hub.challenge">>, Req3),

    lager:info("Handling request with params:~nmode: ~s,~nverify token: ~s,~nchallenge: ~s", [Mode, VerifyToken, Challenge]),
    case {Mode, VerifyToken, Challenge} of
        {_, _, undefined} ->
            lager:warning("Parameter 'hub.challenge' missing during subscription call from Facebook"),
            cowboy_req:reply(400, Req);
        {?HUB_MODE, ?VERIFICATION_TOKEN, Challenge} ->
            lager:info("Valid subscription received from Facebook"),
            cowboy_req:reply(200, [], Challenge, Req4);
        {_, _, _} ->
            lager:warning("Wrong parameters or missing paramenters during subscription call from Facebook"),
            cowboy_req:reply(400, Req)
    end;

reply(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    case HasBody of
        true ->
            handle_post_with_body(Req);
        false ->
            lager:warning("Update notification without body received"),
            cowboy_req:reply(400, Req)
    end;

reply(_, Req) ->
    lager:warning("HTTP request with invalid method received"),
    cowboy_req:reply(405, Req).

handle_post_with_body(Req) ->
    {XHubSignature, Req2} = cowboy_req:header(<<"x-hub-signature">>, Req),
    {ok, [{Payload, true}], Req3} = cowboy_req:body_qs(Req2),
    lager:info("Received update: ~p", [Payload]),

    case is_valid(XHubSignature, Payload) of
        true ->
            Update = jsx:decode(Payload),
            lager:info("Received request is facebook update: ~p", [Update]),

            {AppName, Req4} = cowboy_req:binding(app_name, Req3),
            lager:info("App name is: ~s", [AppName]),

            spawn(fun() -> fetcher:fetch(AppName, Update) end),

            cowboy_req:reply(200, [], <<"">>, Req4);
        false ->
            lager:warning("Update notification with invalid signature received."),
            cowboy_req:reply(400, Req)
    end.

is_valid(XHubSignature, Payload) ->
    {ok, AppSecret} = application:get_env(facebook_listener, app_secret),
    <<Mac:160/integer>> = crypto:hmac(sha, AppSecret, Payload),
    Signature = lists:flatten(io_lib:format("sha1=~40.16.0b", [Mac])),
    binary_to_list(XHubSignature) =:= Signature.

terminate(_Reason, _Req, _State) ->
    ok.

-ifdef(TEST).
    -compile(export_all).
-endif.
