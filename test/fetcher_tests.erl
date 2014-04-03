-module(fetcher_tests).
-include_lib("eunit/include/eunit.hrl").

user_entries_test() ->
    Update = update_with_user_entries(),
    Entries = fetcher:entries_in(Update),

    ?assertEqual([{<<"123456789">>, [<<"likes">>], 1396550005}], Entries).

not_user_entries_test() ->
    Update = update_without_user_entries(),
    Entries = fetcher:entries_in(Update),

    ?assertEqual([], Entries).

update_with_user_entries() ->
    [{<<"object">>,<<"user">>},{<<"entry">>,[[{<<"uid">>,<<"123456789">>},{<<"id">>,<<"123456789">>},{<<"time">>,1396550005},{<<"changed_fields">>,[<<"likes">>]}]]}].

update_without_user_entries() ->
    [{<<"object">>,<<"page">>},{<<"entry">>,[[{<<"any">>,<<"any">>},{<<"any">>,<<"any">>},{<<"time">>,1396550005},{<<"changed_fields">>,[<<"anything">>]}]]}].
