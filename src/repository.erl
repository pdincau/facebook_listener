-module(repository).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-export([get_access_token/2]).

-define(SERVER, ?MODULE).

-record(state, {client}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({access_token, {_AppName, _UserId}}, _From, #state{client=Client} = State) ->
    Reply = <<"securitytoken">>,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{client=Client} = _State) ->
    eredis:stop(Client),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_access_token(AppName, UserId) ->
    gen_server:call(?MODULE, {access_token, {AppName, UserId}}).
