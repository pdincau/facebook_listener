-module(queue_bc).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {client}).

start_link() ->
    gen_server:start_link({local, queue}, ?MODULE, [], []).

init([]) ->
    % TODO: make init send gen_cast message to setup connection
    process_flag(trap_exit,true),
    State = case memcached:connect("127.0.0.1", 11211) of
        {ok, Client} ->
            #state{client=Client};
        _Error ->
            %% TODO: send message to self in order to retry connection
            #state{}
    end,
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({push, Msg}, #state{client=Client} = State) ->
    lager:info("New message is: ~p", [Msg]),
    %% TODO: handle errors here
    ok = memcached:set(Client, "social-web-activities", Msg),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{client=Client} = _State) ->
    memcached:disconnect(Client),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

