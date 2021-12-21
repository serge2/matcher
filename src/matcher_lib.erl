%% @author serge
%% @doc @todo Add description to matcher_lib.


-module(matcher_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         is_date/1,
         is_iso8601_datetime/1,
         is_email/1,
         is_language_alpha2/1,
         is_language_alpha3/1,
         is_country_alpha2/1,
         is_country_alpha3/1,
         is_int8/1,
         is_int16/1,
         is_int32/1,
         is_int64/1,
         is_uint8/1,
         is_uint16/1,
         is_uint32/1,
         is_uint64/1
        ]).



%% ====================================================================
%% Internal functions
%% ====================================================================

-spec is_date(binary()) -> boolean().
is_date(Value) when is_binary(Value) ->
    case re:run(Value, "^([0-9]{4})-([0-1]{1}[0-9]{1})-([0-3]{1}[0-9]{1})\\z", [{capture, all_but_first, list}]) of
        {match, [Y, M, D]} ->
            calendar:valid_date(list_to_integer(Y), list_to_integer(M), list_to_integer(D));
        _ ->
            false
    end.


-spec is_iso8601_datetime(binary()) -> boolean().
is_iso8601_datetime(Value) when is_binary(Value) ->
    try
        _ = iso8601:parse_exact(Value),
        true
    catch
        _:_ ->
            false
    end.


-spec is_email(binary()) -> boolean().
is_email(Value) when is_binary(Value) ->
    match == re:run(Value, "^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+([a-z0-9][a-z0-9-]{0,61}[a-z0-9])\\z",
                    [{capture, none}, caseless]).


-spec is_language_alpha2(binary()) -> boolean().
is_language_alpha2(Value) when is_binary(Value) ->
    case Value of
        <<A:8/unsigned,B:8/unsigned>> ->
            A >= $a andalso A =< $z andalso B >= $a andalso B =< $z;
        _ ->
            false
    end.


-spec is_language_alpha3(binary()) -> boolean().
is_language_alpha3(Value) when is_binary(Value) ->
    case Value of
        <<A:8/unsigned,B:8/unsigned,C:8/unsigned>> ->
            A >= $a andalso A =< $z andalso B >= $a andalso B =< $z andalso C >= $a andalso C =< $z;
        _ ->
            false
    end.


-spec is_country_alpha2(binary()) -> boolean().
is_country_alpha2(Value) when is_binary(Value) ->
    case Value of
        <<A:8/unsigned,B:8/unsigned>> ->
            A >= $A andalso A =< $Z andalso B >= $A andalso B =< $Z;
        _ ->
            false
    end.


-spec is_country_alpha3(binary()) -> boolean().
is_country_alpha3(Value) when is_binary(Value) ->
    case Value of
        <<A:8/unsigned,B:8/unsigned,C:8/unsigned>> ->
            A >= $A andalso A =< $Z andalso B >= $A andalso B =< $Z andalso C >= $A andalso C =< $Z;
        _ ->
            false
    end.


-spec is_int8(integer()) -> boolean().
is_int8(Value) when is_integer(Value) ->
    Value >= -128 andalso Value =< 127.


-spec is_int16(integer()) -> boolean().
is_int16(Value) when is_integer(Value) ->
    Value >= -32768 andalso Value =< 32767.


-spec is_int32(integer()) -> boolean().
is_int32(Value) when is_integer(Value) ->
    Value >= -2147483648 andalso Value =< 2147483647.


-spec is_int64(integer()) -> boolean().
is_int64(Value) when is_integer(Value) ->
    Value >= -9223372036854775808  andalso Value =< 9223372036854775807.


-spec is_uint8(integer()) -> boolean().
is_uint8(Value) when is_integer(Value) ->
    Value >= 0 andalso Value =< 255.


-spec is_uint16(integer()) -> boolean().
is_uint16(Value) when is_integer(Value) ->
    Value >= 0 andalso Value =< 65535.


-spec is_uint32(integer()) -> boolean().
is_uint32(Value) when is_integer(Value) ->
    Value >= 0 andalso Value =< 4294967295.


-spec is_uint64(integer()) -> boolean().
is_uint64(Value) when is_integer(Value) ->
    Value >= 0  andalso Value =< 18446744073709551615.






