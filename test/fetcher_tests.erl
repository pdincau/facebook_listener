-module(fetcher_tests).
-include_lib("eunit/include/eunit.hrl").

user_entries_test() ->
    Update = update_with_user_entries(),
    Entries = fetcher:entries_in(Update),

    ?assertEqual([{1335845740, [<<"name">>,<<"picture">>], 232323}, {1234, [<<"friends">>], 232325}], Entries).

not_user_entries_test() ->
    Update = update_without_user_entries(),
    Entries = fetcher:entries_in(Update),

    ?assertEqual([], Entries).

update_with_user_entries() ->
    [{<<"object">>,<<"user">>},{<<"entry">>,[[{<<"uid">>,1335845740},{<<"changed_fields">>,[<<"name">>,<<"picture">>]},{<<"time">>,232323}],[{<<"uid">>,1234},{<<"changed_fields">>,[<<"friends">>]},{<<"time">>,232325}]]}].


update_without_user_entries() ->
    [{<<"object">>,<<"page">>},{<<"entry">>,[[{<<"uid">>,1335845740},{<<"changed_fields">>,[<<"name">>,<<"picture">>]},{<<"time">>,232323}]]}].
