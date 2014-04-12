-module(repo_mem).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, repository}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({access_token, {_AppName, _UserId}}, _From, State) ->
    Reply = <<"securitytoken">>,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
    -compile(export_all).
-endif.

