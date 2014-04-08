-module(queue_mem).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {msgs}).

start_link() ->
    gen_server:start_link({local, queue}, ?MODULE, [], []).

init([]) ->
    {ok, #state{msgs=[]}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({push, Msg}, #state{msgs=Msgs} = State) ->
    lager:info("New message is: ~p", [Msg]),
    NewState = State#state{msgs=[Msg|Msgs]},
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

