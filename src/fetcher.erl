-module(fetcher).

-compile([{parse_transform, lager_transform}]).

-export([fetch/1]).

fetch(AppName) ->
    Address = lists:concat(["http://www.gazzetta.it/", binary_to_list(AppName)]),

    case httpc:request(Address) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            %% For now I just log the title of a generic web page
            Title = extract_title(Body),
            lager:info("The webpage title is: ~p", [Title]);
        Error ->
            lager:warning("Could fetch from url: ~p~n. Response was: ~p", [Error])
    end.

extract_title(Text) ->
    A = string:str(Text, "<title>"),
    B = string:str(Text, "</title>"),
    string:substr(Text, A+7, B-A-7).
