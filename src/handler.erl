%% Feel free to use, reuse and abuse the code in this file.

%% @doc Facebook Real-time updates handler.
-module(handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HUB_MODE, <<"subscribe">>).

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

    {ok, ExpectedToken} = application:get_env(facebook_listener, verification_token),

    error_logger:info_report(["Handling request with params", {mode, "mode"}, {token, "token"}, {challenge, Challenge}]),
    case {Mode, VerifyToken, Challenge} of
        {_, _, undefined} ->
            error_logger:warning_report("Parameter 'hub.challenge' missing during subscription call from Facebook"),
            cowboy_req:reply(400, Req);
        {?HUB_MODE, ExpectedToken, Challenge} ->
            error_logger:info_report("Valid subscription received from Facebook"),
            cowboy_req:reply(200, [], Challenge, Req4);
        {_, _, _} ->
            error_logger:warning_report("Wrong or missing parameters during subscription call from Facebook"),
            cowboy_req:reply(400, Req)
    end;

reply(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    case HasBody of
        true ->
            handle_post_with_body(Req);
        false ->
            cowboy_req:reply(400, Req)
    end;

reply(_, Req) ->
    cowboy_req:reply(405, Req).

handle_post_with_body(Req) ->
    {XHubSignature, Req2} = cowboy_req:header(<<"x-hub-signature">>, Req),
    {ok, [{Payload, true}], Req3} = cowboy_req:body_qs(Req2),

    case is_valid(XHubSignature, Payload) of
        true ->
            Update = jsx:decode(Payload),
            error_logger:info_report(["Received update", {update, Update}]),

            {AppName, Req4} = cowboy_req:binding(app_name, Req3),

            spawn(fun() -> fetcher:handle(AppName, Update) end),

            cowboy_req:reply(200, [], <<"">>, Req4);
        false ->
            error_logger:info_report("Update notification with invalid signature received"),
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
