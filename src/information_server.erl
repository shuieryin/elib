%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Information server for storing basic configs of app.
%%%
%%% @end
%%% Created : 19. Feb 2016 7:08 PM
%%%-------------------------------------------------------------------
-module(information_server).
-author("shuieryin").

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    start/2,
    start_link/1,
    start/1,
    stop/0,
    module_sequence/1,
    stop_server/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).

-define(SERVER, ?MODULE).

-type stop_server_method() :: {module(), atom(), [term()]}.

-record(state, {
    root_sup_name :: module(),
    info_server_name :: atom(),
    stop_server_method = {init, stop, []} :: stop_server_method()
}).

-export_type([
    stop_server_method/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves module sequences for hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec module_sequence(atom()) -> [module()].
module_sequence(InfoServerName) ->
    gen_server:call({global, InfoServerName}, module_sequence).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves module sequences for hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_server(atom()) -> no_return().
stop_server(InfoServerName) ->
    gen_server:cast({global, InfoServerName}, stop_server).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(stop_server_method(), atom()) -> gen:start_ret().
start_link(StopServerMethod, InfoServerName) ->
    gen_server:start_link({global, InfoServerName}, ?MODULE, {StopServerMethod, InfoServerName}, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts server by setting module name as server name without link.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(stop_server_method(), atom()) -> gen:start_ret().
start(StopServerMethod, InfoServerName) ->
    gen_server:start({global, InfoServerName}, ?MODULE, {StopServerMethod, InfoServerName}, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> gen:start_ret().
start_link(InfoServerName) ->
    start_link({init, stop, []}, InfoServerName).

%%--------------------------------------------------------------------
%% @doc
%% Starts server by setting module name as server name without link.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom()) -> gen:start_ret().
start(InfoServerName) ->
    start({init, stop, []}, InfoServerName).

%%--------------------------------------------------------------------
%% @doc
%% Stop server.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init({StopServerMethod :: tuple(), InfoServerName :: atom()}) ->
    {ok, State} |
    {ok, State, timeout() | hibernate} |
    {stop, Reason} |
    ignore when

    State :: #state{},
    Reason :: term().
init({StopServerMethod, InfoServerName}) ->
    io:format("~p starting...", [?MODULE]),
    register(InfoServerName, self()),

    AppNameStr = begin
                     [ProjectPath | _RestPath] = re:split(filename:absname(""), "_build", [{return, list}]),
                     filename:basename(ProjectPath)
                 end,

    RootSupName = list_to_atom(AppNameStr ++ "_sup"),

    State = #state{
        root_sup_name = RootSupName,
        info_server_name = InfoServerName,
        stop_server_method = StopServerMethod
    },

    io:format("started~n"),

    case whereis(list_to_atom(AppNameStr ++ "_SUITE")) of
        undefined ->
            ok;
        TestProcess ->
            TestProcess ! {TestProcess, list_to_atom(AppNameStr ++ "_started")}
    end,

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) ->
    {reply, Reply, NewState} |
    {reply, Reply, NewState, timeout() | hibernate} |
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, Reply, NewState} |
    {stop, Reason, NewState} when

    Request ::
    module_sequence |
    stop_server,

    Reply :: [module()],

    From :: {pid(), Tag :: term()},
    State :: #state{},
    NewState :: State,
    Reason :: term().
handle_call(module_sequence, _From, #state{
    root_sup_name = RootSupName
} = State) ->
    {reply, lists:flatten(gen_sequence(RootSupName)), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request, State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Request ::
    stop |
    stop_server,

    State :: #state{},
    NewState :: State,
    Reason :: term().
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(stop_server, #state{
    stop_server_method = {Mod, Func, Args}
} = State) ->
    erlang:apply(Mod, Func, Args),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info | timeout(), State) ->
    {noreply, NewState} |
    {noreply, NewState, timeout() | hibernate} |
    {stop, Reason, NewState} when

    Info :: term(),
    State :: #state{},
    NewState :: State,
    Reason :: term().
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> ok when
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) ->
    {ok, NewState} |
    {error, Reason} when

    OldVsn :: term() | {down, term()},
    State :: #state{},
    Extra :: term(),
    NewState :: State,
    Reason :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is useful for customising the form and
%% appearance of the gen_server status for these cases.
%%
%% @spec format_status(Opt, StatusData) -> Status
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt, StatusData) -> Status when
    Opt :: 'normal' | 'terminate',
    StatusData :: [PDict | State],
    PDict :: [{Key :: term(), Value :: term()}],
    State :: #state{},
    Status :: term().
format_status(Opt, StatusData) ->
    gen_server:format_status(Opt, StatusData).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generate module sequences for hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_sequence(module()) -> [module()].
gen_sequence(RootSupName) ->
    RootSequences = supervisor:which_children(RootSupName),
    lists:foldl(
        fun(Spec, AccSequence) ->
            case Spec of
                {_ModuleId, _Pid, worker, [ModuleName]} ->
                    LastModuleName = case AccSequence of
                                         [] ->
                                             undefined;
                                         [LModuleName | _RestAccModuleNames] ->
                                             LModuleName
                                     end,
                    case LastModuleName =/= ModuleName of
                        true ->
                            [ModuleName | AccSequence];
                        false ->
                            AccSequence
                    end;
                {ModuleName, _Pid, supervisor, [ModuleName]} ->
                    [ModuleName, gen_sequence(ModuleName) | AccSequence]
            end
        end, [], RootSequences).