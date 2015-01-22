-module(testudp).
-export([run/1]).

run(Msg) ->
    spawn(
      fun() ->
              {ok, Socket} = gen_udp:open(0, [binary]),
              ok = gen_udp:send(Socket, "localhost", 9993, Msg),
              receive
                  {udp, Socket, _, _, Bin} ->
                      io:format("Response: ~p~n",[Bin]),
                      gen_udp:close(Socket)
              after 3000 ->
                        0
              end
      end).

