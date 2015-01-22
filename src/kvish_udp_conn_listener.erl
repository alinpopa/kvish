-module(kvish_udp_conn_listener).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/1]).

-define(SERVER, ?MODULE).
-define(UDP_OPTIONS, [binary, {active, false}]).
-record(server_state, {socket}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    case gen_udp:open(Port, ?UDP_OPTIONS) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            inet:setopts(Socket, [{active, once}]),
            error_logger:info_msg("UDP Server started on port ~p~n", [Port]),
            gen_server:cast(self(), {listen}),
            {ok, #server_state{socket = Socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({listen}, State = #server_state{socket = Socket}) ->
    gen_udp:controlling_process(Socket, self()),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast({close}, State) ->
    {stop, normal, State};
handle_cast({send_to_client, _Msg}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({udp, ClientSocket, ClientAddress, ClientPort, Bin}, State = #server_state{socket = Socket}) ->
    process_request(ClientSocket, ClientAddress, ClientPort, Bin),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:warning_msg("UNKNOWN info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #server_state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
process_request(ClientSocket, ClientAddress, ClientPort, Request) ->
    ProcessingFun =
    fun() ->
            Message = kvish_message_parser:parse(Request),
            error_logger:info_msg("Processing UDP message ~p~n", [Message]),
            ok = gen_udp:send(ClientSocket, ClientAddress, ClientPort, <<"Alright">>)
    end,
    proc_lib:spawn_link(ProcessingFun).
