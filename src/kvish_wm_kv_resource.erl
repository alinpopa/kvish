-module(kvish_wm_kv_resource).
-export([init/1, allowed_methods/2,
         content_types_provided/2, get_value/2,
         content_types_accepted/2, put_value/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(Rd, Ctx) ->
    {['GET', 'PUT'], Rd, Ctx}.

content_types_provided(Rd, Ctx) ->
    {[{"application/octet-stream", get_value}], Rd, Ctx}.

content_types_accepted(Rd, Ctx) ->
    {[{"application/octet-stream", put_value}], Rd, Ctx}.

get_value(Rd, Ctx) ->
    Command = {get, list_to_binary(wrq:path_info(key, Rd))},
    Response = kvish_kv_store:execute(Command),
    case Response of
        <<"ERROR:", _Rest/binary>> ->
            {{halt, 404}, wrq:append_to_response_body(Response, Rd), Ctx};
        _ ->
            {Response, Rd, Ctx}
    end.

put_value(Rd, Ctx) ->
    Key = list_to_binary(wrq:path_info(key, Rd)),
    Value = wrq:req_body(Rd),
    Command = {put, Key, Value},
    Response = kvish_kv_store:execute(Command),
    {true, wrq:append_to_response_body(Response, Rd), Ctx}.

