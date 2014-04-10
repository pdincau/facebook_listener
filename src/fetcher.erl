-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/2]).

-define(BASE_URL, <<"https://graph.facebook.com/{objectid}/{field}?access_token={token}&{params}">>).

fetch(AppName, Update) ->
    Entries = entries_in(Update),
    lager:info("Entries in update are: ~p", [Entries]),
    Results = [fetch_entry(AppName, UserId, Fields, Timestamp) || {UserId, Fields, Timestamp} <- Entries],
    lager:info("Results are: ~p", [Results]),
    push(Results).

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
        {error, Error} ->
            {error, Error};
        Token ->
            %% TODO: Identify better error
            do_fetch(UserId, lists:nth(1,Fields), Token)
            %% handle multiple fields
            %%[do_fetch(UserId, Field, Token) || Field <- Fields]
    end.

do_fetch(UserId, Field, Token) ->
    Url = url_for(UserId, Field, Token, <<"limit=1">>),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            lager:warning("Couldn't fetch from url: ~p~n. Response was: ~p", [Error]),
            %% TODO: Identify better error
            {error, fetch}
    end.

access_token(AppName, UserId) ->
    gen_server:call(repository, {access_token, {AppName, UserId}}).

url_for(UserId, Field, Token, Params) ->
    Url = binary:replace(?BASE_URL, <<"{objectid}">>, UserId),
    Url1 = binary:replace(Url, <<"{field}">>, Field),
    Url2 = binary:replace(Url1, <<"{token}">>, Token),
    Url3 = binary:replace(Url2, <<"{params}">>, Params),
    binary_to_list(Url3).

push(Results) ->
    %% TODO: this can be improved a lot
    IsSuccess = fun(Msg) ->
                    case Msg of
                        {error, _} -> false;
                        _ -> true
                    end
              end,
    Msgs = lists:filter(IsSuccess, Results),
    [gen_server:cast(queue, {push, Msg}) || Msg <- Msgs].

-ifdef(TEST).
    -compile(export_all).
-endif.
