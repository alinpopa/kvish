-module(kvish_kv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    TcpConfig = kvish_config:tcp_config(),
    WmConfig = kvish_config:web_config(),
    TcpPort = proplists:get_value(port, TcpConfig),
    Rest =
        {webmachine_mochiweb, {webmachine_mochiweb, start, [WmConfig]},
        permanent, 5000, worker, [mochiweb_socket_server]},
    Tcp =
        {kvish_tcp_conn_manager, {kvish_tcp_conn_manager, start_link, [TcpPort, 10]},
        permanent, 5000, worker, [kvish_tcp_conn_manager]},
    Processes = [Rest, Tcp],
    {ok, {{one_for_one, 10, 10}, Processes}}.

