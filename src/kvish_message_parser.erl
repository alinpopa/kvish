-module(kvish_message_parser).
-export([parse/1]).

-spec parse(binary()) ->
    {error, atom()} |
    {get, Key :: binary()} |
    {put, Key :: binary(), Value :: binary()}.
parse(<<>>) ->
    {error, empty};
parse(Bin) ->
    case parse_command(trim(Bin)) of
        {ok, Command, Rest} ->
            case parse_key(Rest) of
                {ok, Key, _Value} when Command =:= get ->
                    {get, Key};
                {ok, Key, Value} when Command =:= put ->
                    {put, Key, Value};
                {ok, Key} when Command =:= get ->
                    {get, Key};
                Els ->
                    Els
            end;
        Els ->
            Els
    end.

%% @private
trim(<<>>) ->
    <<>>;
trim(Bin) ->
    BinSize = size(Bin) - 1,
    <<BinHead:BinSize/binary, BinTail>> = Bin,
    case BinTail of
        10 -> trim(BinHead);
        13 -> trim(BinHead);
        _ -> Bin
    end.

%% @private
parse_command(Bin) ->
    case binary:split(Bin, <<32>>) of
        [Head, Rest] ->
            case to_command(Head) of
                {ok, Command} ->
                    {ok, Command, Rest};
                Els ->
                    Els
            end;
        _ ->
            {error, invalid_message}
    end.

%% @private
parse_key(Bin) ->
    case binary:split(Bin, <<32>>) of
        [Key, Rest] ->
            {ok, Key, Rest};
        [Key] ->
            {ok, Key};
        _ ->
            {error, invalid_message}
    end.

%% @private
to_command(BinCommand) ->
    case string:to_upper(binary_to_list(BinCommand)) of
        "GET" -> {ok, get};
        "PUT" -> {ok, put};
        _ -> {error, invalid_command}
    end.

