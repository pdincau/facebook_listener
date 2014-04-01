-module(handler_tests).
-include_lib("eunit/include/eunit.hrl").

is_valid_test() ->
    XHubSignature = <<"sha1=534985d2be5f2df69cae7cc5e23be204add4f499">>,
    Payload = <<"{\"object\":\"user\",\"entry\":[{\"uid\":1335845740,\"changed_fields\":[\"name\",\"picture\"],\"time\":232323},{\"uid\":1234,\"changed_fields\":[\"friends\"],\"time\":232325}]}">>,

    ?assert(handler:is_valid(XHubSignature, Payload)).


