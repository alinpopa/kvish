-module(kvish_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    TcpConfig = kvish_config:tcp_config(),
    WmConfig = kvish_config:web_config(),
    UdpConfig = kvish_config:udp_config(),
    TcpPort = proplists:get_value(port, TcpConfig),
    UdpPort = proplists:get_value(port, UdpConfig),
    Rest =
        {webmachine_mochiweb, {webmachine_mochiweb, start, [WmConfig]},
        permanent, 5000, worker, [mochiweb_socket_server]},
    Tcp =
        {kvish_tcp_conn_manager, {kvish_tcp_conn_manager, start_link, [TcpPort, 10]},
        permanent, 5000, worker, [kvish_tcp_conn_manager]},
    Udp =
        {kvish_udp_conn_listener, {kvish_udp_conn_listener, start_link, [UdpPort]},
        permanent, 5000, worker, [kvish_udp_conn_listener]},
    KVStore =
        {kvish_kv_store, {kvish_kv_store, start_link, []},
         permanent, 5000, worker, [kvish_kv_store]},
    Processes = [KVStore, Rest, Tcp, Udp],
    {ok, {{one_for_one, 10, 10}, Processes}}.

