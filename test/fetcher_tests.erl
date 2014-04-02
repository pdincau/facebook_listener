-module(fetcher_tests).
-include_lib("eunit/include/eunit.hrl").

entries_test() ->
    Update = user_update(),
    Entries = fetcher:entries_in(Update),

    ?assertEqual([{1335845740, [<<"name">>,<<"picture">>], 232323}, {1234, [<<"friends">>], 232325}], Entries).

user_update() ->
    [{<<"object">>,<<"user">>},{<<"entry">>,[[{<<"uid">>,1335845740},{<<"changed_fields">>,[<<"name">>,<<"picture">>]},{<<"time">>,232323}],[{<<"uid">>,1234},{<<"changed_fields">>,[<<"friends">>]},{<<"time">>,232325}]]}].
