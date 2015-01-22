-module(kvish_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Rest = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [kvish_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Rest],
    {ok, { {one_for_one, 10, 10}, Processes} }.
