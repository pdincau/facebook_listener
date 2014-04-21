-module(parser).

-export([entries_in/1, values_in/1, next/1]).

entries_in(Update) ->
    case Update of
        [{<<"object">>, <<"user">>}, {<<"entry">>, Entries}] ->
            Entries;
        _ ->
            []
    end.

values_in(Entry) ->
    UId = proplists:get_value(<<"uid">>, Entry),
    Time = proplists:get_value(<<"time">>, Entry),
    ChangedFields = proplists:get_value(<<"changed_fields">>, Entry),
    {UId, ChangedFields, Time}.

next(Activity) when is_binary(Activity) ->
    Paging = proplists:get_value(<<"paging">>, jsx:decode(Activity)),
    proplists:get_value(<<"next">>, Paging);

next(Activity) ->
    BinActivity = list_to_binary(Activity),
    Paging = proplists:get_value(<<"paging">>, jsx:decode(BinActivity)),
    proplists:get_value(<<"next">>, Paging).
