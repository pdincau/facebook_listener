-module(repo_redis).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(IDENTIFIER, <<"{app_name}:facebook:{user_id}">>).

-record(state, {client}).

start_link() ->
    gen_server:start_link({local, repository}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({access_token, {AppName, UserId}}, _From, #state{client=_Client} = State) ->
    Identifier = identifier(AppName, UserId),
    lager:info("Identifier is: ~p", [Identifier]),
    Reply = <<"securitytoken">>,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{client=_Client} = _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
identifier(AppName, UserId) ->
    Bin = binary:replace(?IDENTIFIER, <<"{app_name}">>, AppName),
    Bin1 = binary:replace(Bin, <<"{user_id}">>, UserId),
    binary_to_list(Bin1).

-ifdef(TEST).
    -compile(export_all).
-endif.

