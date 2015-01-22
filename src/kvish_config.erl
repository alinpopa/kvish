-module(kvish_config).

-export([dispatch/0, web_config/0, tcp_config/0]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {[], kvish_wm_resource, []},
        {["kv"], kvish_wm_kv_resource, []}
    ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, wm_ip),
    {ok, Port} = application:get_env(App, wm_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].

tcp_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Port} = application:get_env(App, tcp_port),
    [{port, Port}].

