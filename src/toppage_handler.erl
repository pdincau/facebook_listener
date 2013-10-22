%% Feel free to use, reuse and abuse the code in this file.

%% @doc Facebook Real-time updates handler.
-module(toppage_handler).

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
    
    lager:info("Handling get request with params:~nmode: ~s,~nverify token: ~s,~nchallenge: ~s", [Mode, VerifyToken, Challenge]),
    case {Mode, VerifyToken, Challenge} of
        {_, _, undefined} ->
            lager:warning("Parameter 'hub.challenge' missing during subscription call from Facebook"),
            cowboy_req:reply(404, Req);
        {?HUB_MODE, ?VERIFICATION_TOKEN, Challenge} ->
            lager:info("Valid subscription received from Facebook"),
            cowboy_req:reply(200, [], Challenge, Req4);
        {_, _, _} ->
            lager:warning("Wrong parameters or missing paramenters during subscription call from Facebook"),
            cowboy_req:reply(404, Req)
    end;

reply(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    case HasBody of
        true ->
            handle_post_with_body(Req);
        false ->
            lager:warning("Update notification without body received"),
            cowboy_req:reply(404, Req)
    end;

reply(_, Req) ->
	%% Method not allowed.
    lager:warning("Wrong HTTP request received"),
    cowboy_req:reply(405, Req).

handle_post_with_body(Req) ->
    {ContentType, Req2} = cowboy_req:header(<<"content-type">>, Req),
    lager:info("Post request received has content type: ~s", [ContentType]),
    case ContentType of
        <<"application/json">> ->
            {ok, PostVals, Req3} = cowboy_req:body_qs(Req2),
            handle_post_body(Req3, PostVals);
        _ ->
            lager:warning("Update notification with wrong content-type"),
            cowboy_req:reply(404, Req)
    end.

handle_post_body(Req, PostVals) ->
    case PostVals of
        [{PostVal, true}] ->
            case jsx:is_json(PostVal) of
                true ->
                    FacebookUpdate = jsx:decode(PostVal),
                    lager:info("Received request is facebook update: ~p", [FacebookUpdate]),
                    cowboy_req:reply(200, [], <<"">>, Req);
                false ->
                    lager:warning("Post request received is not a valid json"),
                    cowboy_req:reply(404, Req)
            end;
        _Any ->
            lager:warning("Post request received has more than one parameter"),
            cowboy_req:reply(404, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.
