-module(repo_bc_tests).
-include_lib("eunit/include/eunit.hrl").

identifier_test() ->
    Identifier = repo_bc:identifier(<<"an_app_name">>, <<"a_user_id">>),

    ?assertEqual("an_app_name:facebook:a_user_id", Identifier).
