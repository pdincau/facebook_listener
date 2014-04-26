-module(repo_mem).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {timestamp}).

start_link() ->
    gen_server:start_link({local, repository}, ?MODULE, [], []).

init([]) ->
    {ok, #state{timestamp = <<"1398006309">>}}.

handle_call({access_token, {_AppName, _UserId}}, _From, State) ->
    Reply = {token, <<"securitytoken">>},
    {reply, Reply, State};

handle_call({last_timestamp, _UserId}, _From, #state{timestamp=Timestamp} = State) ->
    {reply, Timestamp, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({new_timestamp, _UserId, Timestamp}, State) ->
    NewState = State#state{timestamp=Timestamp},
    {noreply, NewState};

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
