-module(kvish_message_parser_test).
-include_lib("eunit/include/eunit.hrl").

-define(TESTMOD, kvish_message_parser).

parse_test_() ->
    [{"should return invalid_command error for wrong command",
      fun() ->
              ?assertEqual({error, invalid_command},
                           ?TESTMOD:parse(<<"this is an invalid command">>))
      end},
     {"should return invalid message error for wrong command",
      fun() ->
              ?assertEqual({error, invalid_message},
                           ?TESTMOD:parse(<<"getisaninvalidmessage">>)),
              ?assertEqual({error, invalid_message},
                           ?TESTMOD:parse(<<"get">>))
      end},
    {"should successfully parse valid messages",
     fun() ->
             ?assertEqual({get, <<"somekey">>},
                          ?TESTMOD:parse(<<"get somekey some value in here">>)),
             ?assertEqual({put, <<"somekey">>, <<"some value in here">>},
                          ?TESTMOD:parse(<<"put somekey some value in here">>)),
             ?assertEqual({get, <<"somekey">>},
                          ?TESTMOD:parse(<<"get somekey">>))
     end}].

