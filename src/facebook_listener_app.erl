%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(facebook_listener_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(C_ACCEPTORS, 100).

%% API.

start(_StartType, _StartArgs) ->
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    Port = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    facebook_listener_sup:start_link().

stop(_State) ->
    ok.

routes() ->
    [{'_', [{"/:app_name", handler, []}]}].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            {Port, _} = string:to_integer(Other),
            Port
    end.
