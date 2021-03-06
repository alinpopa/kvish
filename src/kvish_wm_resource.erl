-module(kvish_wm_resource).
-export([init/1, allowed_methods/2,
         content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(Rd, Ctx) ->
    {['GET'], Rd, Ctx}.

content_types_provided(Rd, Ctx) ->
    {[{"application/json", to_json}], Rd, Ctx}.

to_json(Rd, Ctx) ->
    {<<"{\"status\": \"ok\"}">>, Rd, Ctx}.

