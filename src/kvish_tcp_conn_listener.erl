-module(kvish_tcp_conn_listener).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/1]).

-define(SERVER, ?MODULE).
-define(LISTENER_MANAGER, kvish_tcp_conn_manager).
-record(conn_state, {listening_socket, client_socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    gen_server:cast(self(), {listen}),
    {ok, #conn_state{listening_socket = Socket}}.

handle_cast({listen}, State) ->
    {ok, ClientSocket} = gen_tcp:accept(State#conn_state.listening_socket),
    error_logger:info_msg("Client connected (concurrent no. of processes ~p)~n", [length(processes())]),
    gen_tcp:controlling_process(ClientSocket, self()),
    inet:setopts(ClientSocket, [{active, once}]),
    ?LISTENER_MANAGER:detach(),
    {noreply, State#conn_state{client_socket = ClientSocket}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({tcp, _ClientSocket, BinMsg}, State) ->
    Message = kvish_message_parser:parse(BinMsg),
    inet:setopts(State#conn_state.client_socket, [{active, once}]),
    Response = kvish_kv_store:execute(Message),
    gen_tcp:send(State#conn_state.client_socket, [Response]),
    {noreply, State};
handle_info({tcp_closed, _ClientSocket}, State) ->
    error_logger:info_msg("Connection closed by client (concurrent no. of processes ~p)~n", [length(processes())]),
    {stop, normal, State};
handle_info(Info, State) ->
    error_logger:warning_msg("UNKNOWN info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

