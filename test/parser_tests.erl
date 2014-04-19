-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

entries_in_test() ->
    Update = update_with_user_entries(),

    ?assertEqual(entries(), parser:entries_in(Update)).

values_in_test() ->
    [Entry|_] = entries(),
    EntryValues = entry_values(),

    ?assertEqual(EntryValues, parser:values_in(Entry)).

next_test() ->
    Activity = activity(),

    ?assertEqual(<<"next target">>, parser:next(Activity)).

update_with_user_entries() ->
    [{<<"object">>,<<"user">>},{<<"entry">>,[[{<<"uid">>,<<"123456789">>},{<<"id">>,<<"123456789">>},{<<"time">>,1396550005},{<<"changed_fields">>,[<<"likes">>]}]]}].

entries() ->
    [[{<<"uid">>,<<"123456789">>},{<<"id">>,<<"123456789">>},{<<"time">>,1396550005},{<<"changed_fields">>,[<<"likes">>]}]].

entry_values() ->
    {<<"123456789">>, [<<"likes">>], 1396550005}.

activity() ->
    "{\"data\":[{\"category\":\"any\",\"name\":\"any\",\"created_time\":\"any\",\"id\":\"any\"}],\"paging\":{\"cursors\":{\"after\":\"any\",\"before\":\"any\"},\"next\":\"next target\"}}".
