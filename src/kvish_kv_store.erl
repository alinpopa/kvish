-module(kvish_kv_store).
-behaviour(gen_server).

-export([start_link/0, execute/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, kv_data).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec execute({error, atom()} |
              {get, Key :: binary()} |
              {put, Key :: binary(), Value :: binary()}) -> binary().
execute({error, Reason}) when is_atom(Reason) ->
    Error = <<"ERROR:">>,
    BinReason = list_to_binary(atom_to_list(Reason)),
    <<Error/binary, BinReason/binary>>;
execute({get, Key}) ->
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            << <<"VALUE:">>/binary, Value/binary >>;
        [] ->
            <<"ERROR:key_not_found">>
    end;
execute(Message = {put, _Key, _Value}) ->
    gen_server:cast(?MODULE, Message),
    <<"OK">>.

init([]) ->
    ets:new(?TABLE_NAME, [protected, named_table, {read_concurrency, true}]),
    {ok, []}.

handle_cast({put, Key, Value}, State) ->
    ets:insert(?TABLE_NAME, {Key, Value}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

