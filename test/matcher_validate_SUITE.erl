%%%-------------------------------------------------------------------
%%% File    : matcher_validate_SUITE.erl
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : 
%%%
%%% Created : 
%%%-------------------------------------------------------------------
-module(matcher_validate_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [
     test_unknown_type,
     test_const_positive,
     test_const_negative,
     test_const_bad_descriptor,
     test_enum_positive,
     test_enum_negative,
     test_enum_bad_descriptor,
     test_binary_positive,
     test_binary_negative,
     test_binary_bad_descriptor,
     test_utf8_binary_positive,
     test_utf8_binary_negative,
     test_utf8_binary_bad_descriptor,
     test_utf8_binary_pattern_bad_descriptor,
     test_utf8_binary_pattern_positive,
     test_utf8_binary_pattern_negative,
     test_utf8_binary_pattern_bad_descriptor,
     test_utf8_binary_date_positive,
     test_utf8_binary_date_negative,
     test_utf8_binary_datetime_positive,
     test_utf8_binary_datetime_negative,
     test_utf8_binary_email_positive,
     test_utf8_binary_email_negative,
     test_utf8_binary_lang_alpha2_positive,
     test_utf8_binary_lang_alpha2_negative,
     test_utf8_binary_lang_alpha3_positive,
     test_utf8_binary_lang_alpha3_negative,
     test_utf8_binary_country_alpha2_positive,
     test_utf8_binary_country_alpha2_negative,
     test_utf8_binary_country_alpha3_positive,
     test_utf8_binary_country_alpha3_negative,
     test_integer_positive,
     test_integer_negative,
     test_integer_bad_descriptor,
     test_integer_int8,
     test_integer_int16,
     test_integer_int32,
     test_integer_int64,
     test_integer_uint8,
     test_integer_uint16,
     test_integer_uint32,
     test_integer_uint64,
     test_number_positive,
     test_number_negative,
     test_number_bad_descriptor,
     test_decimal_binary_positive,
     test_decimal_binary_negative,
     test_decimal_binary_bad_descriptor,
     test_boolean_positive,
     test_boolean_negative,
     test_boolean_bad_descriptor,
     test_term_positive,
     test_term_negative,
     test_term_bad_descriptor,
     test_custom_positive,
     test_custom_negative,
     test_custom_bad_descriptor,
     test_tuple_positive,
     test_tuple_negative,
     test_tuple_bad_descriptor,
     test_list_positive,
     test_list_negative,
     test_list_bad_descriptor,
     test_map_positive,
     test_map_negative,
     test_map_bad_descriptor,
     test_alt_positive,
     test_alt_negative,
     test_alt_bad_descriptor,
     test_alt_simple_positive,
     test_alt_simple_negative,
     test_alt_simple_bad_descriptor,
     test_converters_positive,
     test_converters_negative,
     test_converters_bad_descriptor
    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
%% test_is_date() -> 
%%     [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
test_unknown_type(_Config) ->
    ?assertMatch({error, {bad_descriptor, [], unsupported_type}},
                 matcher:validate(null, #{type => some_unknown_type})),
    ok.

test_const_positive(_Config) ->
    ?assertMatch({ok, null}, matcher:validate(null, #{type => const, nullable => true, value => 1})),
    ?assertMatch({ok, 1}, matcher:validate(1, #{type => const, nullable => false, value => 1})),
    ?assertMatch({ok, some_atom}, matcher:validate(some_atom, #{type => const, value => some_atom})),
    ?assertMatch({ok, 10}, matcher:validate(10, #{type => const, value => 10})),
    ?assertMatch({ok, #{a := b}}, matcher:validate(#{a => b}, #{type => const, value => #{a => b}})),
    ok.

test_const_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], not_match}}, matcher:validate(null, #{type => const, value => 1})),
    ?assertMatch({error, {not_valid, [], not_match}}, matcher:validate(null, #{type => const, nullable => false, value => 1})),
    ?assertMatch({error, {not_valid, [], not_match}}, matcher:validate(some_atom2, #{type => const, value => some_atom1})),
    ?assertMatch({error, {not_valid, [], not_match}}, matcher:validate(10, #{type => const, value => 11})),
    ?assertMatch({error, {not_valid, [], not_match}}, matcher:validate(#{a => b, c => d}, #{type => const, value => #{a => b}})),
    ok.

test_const_bad_descriptor(_Config) ->
    ?assertMatch({error, {bad_descriptor ,[], value_missed}},
                 matcher:validate(some_atom2, #{type => const})), % The value parameter must be specified
    ?assertMatch({error, {bad_descriptor, [], invalid_nullable}},
                 matcher:validate(1, #{type => const, value => 1, nullable => 3})), % The nullable must be boolean 
    ok.

test_enum_positive(_Config) ->
    ?assertMatch({ok, null}, matcher:validate(null, #{type => enum, nullable => true, values => [1, some_atom, #{}]})),
    ?assertMatch({ok, some_atom}, matcher:validate(some_atom, #{type => enum, nullable => false, values => [1, some_atom, #{}]})),
    ?assertMatch({ok, some_atom}, matcher:validate(some_atom, #{type => enum, values => [1, some_atom, #{}]})),
    ?assertMatch({ok, 1}, matcher:validate(1, #{type => enum, values => [1, some_atom, #{}]})),
    ?assertMatch({ok, #{}}, matcher:validate(#{}, #{type => enum, values => [1, some_atom, #{}]})),
    ok.

test_enum_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(null, #{type => enum, values => [1, some_atom, #{}]})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(null, #{type => enum, nullable => false, values => [1, some_atom, #{}]})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(some_atom2, #{type => enum, values => [1, some_atom, #{}]})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(10, #{type => enum, values => [1, some_atom, #{}]})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(#{a => b}, #{type => enum, values => [1, some_atom, #{}]})),
    ok.

test_enum_bad_descriptor(_Config) ->
    ?assertMatch({error, {bad_descriptor, [], invalid_values}},
                 matcher:validate(some_atom2, #{type => enum, values => {1, some_atom, #{}}})), % The values must be a list
    ?assertMatch({error, {bad_descriptor, [], values_missed}},
                 matcher:validate(some_atom2, #{type => enum})), % The values parameter must be specified
    ?assertMatch({error, {bad_descriptor, [], invalid_values}},
                 matcher:validate(some_atom2, #{type => enum, values => []})), % The values parameter length must be > 0 
    ?assertMatch({error, {bad_descriptor,[], invalid_nullable}},
                 matcher:validate(some_atom2, #{type => enum, values => [1], nullable => "true"})), % The nullable must be boolean 
    ok.

test_binary_positive(_Config) ->
    ?assertMatch({ok, null}, matcher:validate(null, #{type => binary, nullable => true})),
    ?assertMatch({ok, <<1,2,3>>}, matcher:validate(<<1,2,3>>, #{type => binary, nullable => false})),
    ?assertMatch({ok, <<>>}, matcher:validate(<<>>, #{type => binary})),
    ?assertMatch({ok, <<>>}, matcher:validate(<<>>, #{type => binary, min_size => 0})),
    ?assertMatch({ok, <<>>}, matcher:validate(<<>>, #{type => binary, max_size => 0})),
    ?assertMatch({ok, <<1,2,3>>}, matcher:validate(<<1,2,3>>, #{type => binary})),
    ?assertMatch({ok, <<1,2,3>>}, matcher:validate(<<1,2,3>>, #{type => binary, min_size => 3})),
    ?assertMatch({ok, <<1,2,3,4>>}, matcher:validate(<<1,2,3,4>>, #{type => binary, min_size => 3})),
    ?assertMatch({ok, <<1,2,3>>}, matcher:validate(<<1,2,3>>, #{type => binary, max_size => 3})),
    ?assertMatch({ok, <<1,2>>}, matcher:validate(<<1,2>>, #{type => binary, max_size => 3})),
    ?assertMatch({ok, <<1,2>>}, matcher:validate(<<1,2>>, #{type => binary, min_size => 2, max_size => 3})),
    ?assertMatch({ok, <<1,2,3>>}, matcher:validate(<<1,2,3>>, #{type => binary, min_size => 1, max_size => 3})),
    ok.

test_binary_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(null, #{type => binary})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(null, #{type => binary, nullable => false})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(some_atom2, #{type => binary})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(1, #{type => binary})),
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(<<1,2,3>>, #{type => binary, min_size => 4})),
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<1,2,3>>, #{type => binary, max_size => 2})),
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<1,2,3>>, #{type => binary, min_size => 1, max_size => 2})),
    ok.

test_binary_bad_descriptor(_Config) ->
    % The min_size must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_min_size}},
                 matcher:validate(<<1>>, #{type => binary, min_size => 1.0})),
    % The min_size must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_min_size}},
                 matcher:validate(<<1>>, #{type => binary, min_size => -1})),
    % The max_size must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_max_size}},
                 matcher:validate(<<1>>, #{type => binary, max_size => 1.0})),
    % The max_size must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_max_size}},
                 matcher:validate(<<1>>, #{type => binary, max_size => -1})),
    % The nullable must be boolean
    ?assertMatch({error, {bad_descriptor, [], invalid_nullable}},
                 matcher:validate(<<1>>, #{type => binary, nullable => "true"})),
    ok.

test_utf8_binary_positive(_Config) ->
    ?assertMatch({ok, null}, matcher:validate(null, #{type => utf8_binary, nullable => true})),
    ?assertMatch({ok, <<"Hello">>}, matcher:validate(<<"Hello">>, #{type => utf8_binary, nullable => false})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary})),
    ?assertMatch({ok, <<>>}, matcher:validate(<<>>, #{type => utf8_binary, min_length => 0})),
    ?assertMatch({ok, <<>>}, matcher:validate(<<>>, #{type => utf8_binary, max_length => 0})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 6})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 5})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 6})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 7})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 2, max_length => 6})),
    ?assertMatch({ok, <<"Привет"/utf8>>}, matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 6, max_length => 7})),
    ok.

test_utf8_binary_negative(_Config) ->
    % Check null value
    ?assertMatch({error, {not_valid, [], wrong_type}}, matcher:validate(null, #{type => utf8_binary})),
    % Check null value with nullable = false
    ?assertMatch({error, {not_valid, [], wrong_type}}, matcher:validate(null, #{type => utf8_binary, nullable => false})),
    % A value must be binary
    ?assertMatch({error, {not_valid, [], wrong_type}}, matcher:validate(some_atom2, #{type => utf8_binary})),
    ?assertMatch({error, {not_valid, [], wrong_type}}, matcher:validate(1, #{type => utf8_binary})),
    % The value too short
    ?assertMatch({error, {not_valid, [], too_short}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 7})),
    % The value too long
    ?assertMatch({error, {not_valid, [], too_long}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 5})),
    % The value length out of bound
    ?assertMatch({error, {not_valid, [], too_long}},
                 matcher:validate(<<"Привет, медвед"/utf8>>, #{type => utf8_binary, min_length => 1, max_length => 7})),
    ok.

test_utf8_binary_bad_descriptor(_Config) ->
    % The min_size must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_min_length}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 1.0})),
    % The min_size must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_min_length}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => -1})),
    % The max_size must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_max_length}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 1.0})),
    % The max_size must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_max_length}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => -1})),
    % The nullable must be boolean
    ?assertMatch({error, {bad_descriptor, [], invalid_nullable}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, nullable => "true"})),
    % Unknown format
    ?assertMatch({error, {bad_descriptor, [], unknown_format}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, format => xxx})),
    ok.


test_utf8_binary_pattern_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, pattern => <<"^[0-9]+">>})),
    ?assertMatch({ok, <<"201">>},
                 matcher:validate(<<"201">>, #{type => utf8_binary, nullable => false, pattern => <<"^[0-9]+">>})),
    ?assertMatch({ok, <<"1234567890">>},
                 matcher:validate(<<"1234567890">>, #{type => utf8_binary, pattern => <<"^[0-9]+">>})),
    ?assertMatch({ok, <<>>},
                 matcher:validate(<<>>, #{type => utf8_binary, min_length => 0, pattern => <<"^[0-9]*">>})),
    ?assertMatch({ok, <<>>},
                 matcher:validate(<<>>, #{type => utf8_binary, max_length => 0, pattern => <<"^[0-9]*">>})),
    ?assertMatch({ok, <<"123456">>},
                 matcher:validate(<<"123456">>, #{type => utf8_binary, min_length => 6, pattern => <<"^[0-9]*">>})),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => utf8_binary, max_length => 5, pattern => <<"^[0-9]*">>})),
    ok.


test_utf8_binary_pattern_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(<<>>, #{type => utf8_binary, pattern => <<"^[0-9]+">>})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => <<"^[0-9]+">>})),
    % The value must be a binary string
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("Привет", #{type => utf8_binary, pattern => <<"^[0-9]+">>})),
    ok.


test_utf8_binary_pattern_bad_descriptor(_Config) ->
    % The pattern must be a binary string
    ?assertMatch({error, {bad_descriptor, [], invalid_pattern}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => "^[0-9]+"})),
    % Invalid pattern
    ?assertMatch({error, {bad_descriptor, [], invalid_pattern}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => "^0-9]+"})),
    % The pattern must be a binary string
    ?assertMatch({error, {bad_descriptor, [], invalid_pattern}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => 3})),
    % The pattern must be a binary string
    ?assertMatch({error, {bad_descriptor, [], invalid_pattern}},
                 matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => null})),
    ok.

test_utf8_binary_date_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => date})),
    ?assertMatch({ok, <<"1976-01-08">>},
                 matcher:validate(<<"1976-01-08">>, #{type => utf8_binary, nullable => false, format => date})),
    ?assertMatch({ok, <<"1976-01-08">>},
                 matcher:validate(<<"1976-01-08">>, #{type => utf8_binary, format => date})),
    ?assertMatch({ok, <<"1980-02-29">>},
                 matcher:validate(<<"1980-02-29">>, #{type => utf8_binary, format => date})),
    ?assertMatch({ok, <<"2013-12-31">>},
                 matcher:validate(<<"2013-12-31">>, #{type => utf8_binary, format => date})),
    ok.


test_utf8_binary_date_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => date})),
    % Just wrong format
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1976:01:08">>, #{type => utf8_binary, format => date})),
    % A datitime is not date
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1976-01-08T02:13:10">>, #{type => utf8_binary, format => date})),
    % It is not a leap year
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1981-02-29">>, #{type => utf8_binary, format => date})),
    % The value must be a binary string
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("1976-01-08", #{type => utf8_binary, format => date})),
    ok.

test_utf8_binary_datetime_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T22:59:59">>},
                 matcher:validate(<<"1976-01-08T22:59:59">>, #{type => utf8_binary, nullable => false, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T22:59:60">>},
                 matcher:validate(<<"1976-01-08T22:59:60">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T00:59:32Z">>},
                 matcher:validate(<<"1976-01-08T00:59:32Z">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T22:59:59.123456">>},
                 matcher:validate(<<"1976-01-08T22:59:59.123456">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T22:59:59.3">>},
                 matcher:validate(<<"1976-01-08T22:59:59.3">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T00:59:32.453Z">>},
                 matcher:validate(<<"1976-01-08T00:59:32.453Z">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T00:59">>},
                 matcher:validate(<<"1976-01-08T00:59">>, #{type => utf8_binary, format => datetime})),
%%     ?assertMatch({ok, <<"1976-01-08T00:59Z">>},
%%                  matcher:validate(<<"1976-01-08T00:59Z">>, #{type => utf8_binary, format => datetime})),
    ?assertMatch({ok, <<"1976-01-08T23">>},
                 matcher:validate(<<"1976-01-08T23">>, #{type => utf8_binary, format => datetime})),
%%     ?assertMatch({ok, <<"1976-01-08T23Z">>},
%%                   matcher:validate(<<"1976-01-08T23Z">>, #{type => utf8_binary, format => datetime})),
    ok.


test_utf8_binary_datetime_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => datetime})),
    % Just wrong format
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1976:01:08T23:01:22">>, #{type => utf8_binary, format => datetime})),
    % A date is not datetime
%%     ?assertMatch({error, {not_valid, [], incorrect_format}},
%%                  matcher:validate(<<"1976-01-08">>, #{type => utf8_binary, format => datetime})),
    % Its is not a leap year
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1981-02-29T10:01:34">>, #{type => utf8_binary, format => datetime})),
    % Incorrect month
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"1981-13-20T10:01:34">>, #{type => utf8_binary, format => datetime})),
    % Incorrect hours
%%     ?assertMatch({error, {not_valid, [], incorrect_format}},
%%                  matcher:validate(<<"1981-10-20T24:01:34">>, #{type => utf8_binary, format => datetime})),
    % Incorrect minutes
%%     ?assertMatch({error, {not_valid, [], incorrect_format}},
%%                  matcher:validate(<<"1981-10-20T23:88:34">>, #{type => utf8_binary, format => datetime})),
    % Incorrect seconds
%%     ?assertMatch({error, {not_valid, [], incorrect_format}},
%%                  matcher:validate(<<"1981-10-20T23:08:78">>, #{type => utf8_binary, format => datetime})),
    % The value must be a binary string
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("1976-01-08T12:09:22Z", #{type => utf8_binary, format => datetime})),
    ok.


test_utf8_binary_email_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => email})),
    ?assertMatch({ok, <<"john.doe@localhost.localdomain">>},
                 matcher:validate(<<"john.doe@localhost.localdomain">>, #{type => utf8_binary, nullable => false, format => email})),
    ?assertMatch({ok, <<"j@d24.org">>},
                 matcher:validate(<<"j@d24.org">>, #{type => utf8_binary, format => email})),
    ?assertMatch({ok, <<"john.doe@24h.org">>},
                 matcher:validate(<<"john.doe@24h.org">>, #{type => utf8_binary,format => email})),
    ?assertMatch({ok, <<"j0308@fem.hq.uk">>},
                 matcher:validate(<<"j0308@fem.hq.uk">>, #{type => utf8_binary, format => email})),
    ?assertMatch({ok, <<"John-Doe@HIGH-CASE.DOMAIN.NET">>},
                 matcher:validate(<<"John-Doe@HIGH-CASE.DOMAIN.NET">>, #{type => utf8_binary, format => email})),
    ok.


test_utf8_binary_email_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => email})),
    % At least two subdomains expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"john.doe@localhost">>, #{type => utf8_binary, nullable => false, format => email})),
    % Very long domin
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"john.doe@veeeeeeeeeeeeeeeeeeeeeeeeeeeerrylooooooooooooooooooooooooooooong.domain">>,
                                  #{type => utf8_binary, format => email})),
    % Subdomain must not be started with "-"
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"john.doe@-24h.org">>, #{type => utf8_binary, format => email})),
    % Root domain must be two or more symbols
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"john.doe@localhost.l">>, #{type => utf8_binary, format => email})),
    % Incorrect format
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"john.doe@localhost@localdomain">>, #{type => utf8_binary, format => email})),
    % The value must be a binary string
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("john.doe@localhost.localdomain", #{type => utf8_binary, format => email})),
    ok.

test_utf8_binary_lang_alpha2_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => lang_alpha2})),
    ?assertMatch({ok, <<"uk">>},
                 matcher:validate(<<"uk">>, #{type => utf8_binary, nullable => false, format => lang_alpha2})),
    ?assertMatch({ok, <<"uk">>},
                 matcher:validate(<<"uk">>, #{type => utf8_binary, format => lang_alpha2})),
    ?assertMatch({ok, <<"en">>},
                 matcher:validate(<<"en">>, #{type => utf8_binary,format => lang_alpha2})),
    ok.


test_utf8_binary_lang_alpha2_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => lang_alpha2})),
    % Exactly two symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"ukr">>, #{type => utf8_binary, format => lang_alpha2})),
    % Exactly two symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"u">>, #{type => utf8_binary, format => lang_alpha2})),
    % Must be lowercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UK">>, #{type => utf8_binary, format => lang_alpha2})),
    % Must be lowercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"Uk">>, #{type => utf8_binary, format => lang_alpha2})),
    % Only latin characters allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"u1">>, #{type => utf8_binary, format => lang_alpha2})),
     % No spaces allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"u ">>, #{type => utf8_binary, format => lang_alpha2})),
    ok.


test_utf8_binary_lang_alpha3_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => lang_alpha3})),
    ?assertMatch({ok, <<"ukr">>},
                 matcher:validate(<<"ukr">>, #{type => utf8_binary, nullable => false, format => lang_alpha3})),
    ?assertMatch({ok, <<"ukr">>},
                 matcher:validate(<<"ukr">>, #{type => utf8_binary, format => lang_alpha3})),
    ?assertMatch({ok, <<"eng">>},
                 matcher:validate(<<"eng">>, #{type => utf8_binary,format => lang_alpha3})),
    ok.


test_utf8_binary_lang_alpha3_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => lang_alpha3})),
    % Exactly three symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"uk">>, #{type => utf8_binary, format => lang_alpha3})),
    % Exactly three symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"ukrn">>, #{type => utf8_binary, format => lang_alpha3})),
    % Must be lowercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UKR">>, #{type => utf8_binary, format => lang_alpha3})),
    % Must be lowercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"Ukr">>, #{type => utf8_binary, format => lang_alpha3})),
    % Only latin characters allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"uk1">>, #{type => utf8_binary, format => lang_alpha3})),
    % No spaces allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"uk ">>, #{type => utf8_binary, format => lang_alpha3})),
    ok.


test_utf8_binary_country_alpha2_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => country_alpha2})),
    ?assertMatch({ok, <<"UA">>},
                 matcher:validate(<<"UA">>, #{type => utf8_binary, nullable => false, format => country_alpha2})),
    ?assertMatch({ok, <<"UA">>},
                 matcher:validate(<<"UA">>, #{type => utf8_binary, format => country_alpha2})),
    ?assertMatch({ok, <<"GB">>},
                 matcher:validate(<<"GB">>, #{type => utf8_binary,format => country_alpha2})),
    ok.


test_utf8_binary_country_alpha2_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => country_alpha2})),
    % Exactly two symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UAN">>, #{type => utf8_binary, format => country_alpha2})),
    % Exactly two symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"U">>, #{type => utf8_binary, format => country_alpha2})),
    % Must be uppercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"ua">>, #{type => utf8_binary, format => country_alpha2})),
    % Must be uppercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"Ua">>, #{type => utf8_binary, format => country_alpha2})),
    % Only latin characters allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"U1">>, #{type => utf8_binary, format => country_alpha2})),
    % No spaces allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"U ">>, #{type => utf8_binary, format => country_alpha2})),
    ok.

test_utf8_binary_country_alpha3_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, nullable => true, format => country_alpha3})),
    ?assertMatch({ok, <<"UAN">>},
                 matcher:validate(<<"UAN">>, #{type => utf8_binary, nullable => false, format => country_alpha3})),
    ?assertMatch({ok, <<"UAN">>},
                 matcher:validate(<<"UAN">>, #{type => utf8_binary, format => country_alpha3})),
    ?assertMatch({ok, <<"GBR">>},
                 matcher:validate(<<"GBR">>, #{type => utf8_binary,format => country_alpha3})),
    ok.


test_utf8_binary_country_alpha3_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<>>, #{type => utf8_binary, format => country_alpha3})),
    % Exactly three symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UANK">>, #{type => utf8_binary, format => country_alpha3})),
    % Exactly three symbole expected
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UA">>, #{type => utf8_binary, format => country_alpha3})),
    % Must be uppercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"uan">>, #{type => utf8_binary, format => country_alpha3})),
    % Must be uppercase
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"Uan">>, #{type => utf8_binary, format => country_alpha3})),
    % Only latin characters allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UA1">>, #{type => utf8_binary, format => country_alpha3})),
    % No spaces allowed
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(<<"UA ">>, #{type => utf8_binary, format => country_alpha3})),
    ok.

test_integer_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => integer, nullable => true})),
    ?assertMatch({ok, 12345},
                 matcher:validate(12345, #{type => integer, nullable => false})),
    ?assertMatch({ok, 12345},
                 matcher:validate(12345, #{type => integer})),
    ?assertMatch({ok, 123456789012345678901234567890},
                 matcher:validate(123456789012345678901234567890, #{type => integer})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer})),
    ?assertMatch({ok, -12345},
                 matcher:validate(-12345, #{type => integer})),
    ?assertMatch({ok, -123456789012345678901234567890},
                 matcher:validate(-123456789012345678901234567890, #{type => integer})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, minimum => 0})),
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => integer, minimum => 0})),
    ?assertMatch({ok, 100500},
                 matcher:validate(100500, #{type => integer, minimum => 0})),
    ?assertMatch({ok, 10},
                 matcher:validate(10, #{type => integer, maximum => 10})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, maximum => 10})),
    ?assertMatch({ok, -12345},
                 matcher:validate(-12345, #{type => integer, maximum => 10})),
    ?assertMatch({ok, 2},
                 matcher:validate(2, #{type => integer, minimum => 2, maximum => 4})),
    ?assertMatch({ok, 3},
                 matcher:validate(3, #{type => integer, minimum => 2, maximum => 4})),
    ?assertMatch({ok, 4},
                 matcher:validate(4, #{type => integer, minimum => 2, maximum => 4})),
    ?assertMatch({ok, -3},
                 matcher:validate(-3, #{type => integer, minimum => -3, maximum => -3})),
    ?assertMatch({ok, -8},
                 matcher:validate(-8, #{type => integer, divisible_by => 4})),
    ?assertMatch({ok, -4},
                 matcher:validate(-4, #{type => integer, divisible_by => 4})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, divisible_by => 4})),
    ?assertMatch({ok, 4},
                 matcher:validate(4, #{type => integer, divisible_by => 4})),
    ?assertMatch({ok, 120},
                 matcher:validate(120, #{type => integer, divisible_by => 4})),
    ?assertMatch({ok, 25},
                 matcher:validate(25, #{type => integer, divisible_by => 5, minimum => 24, maximum => 29})),
    ok.

test_integer_negative(_Config) ->
    % A float is not an integer
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => integer})),
    % Mast be integer type
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("12345", #{type => integer})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-1, #{type => integer, minimum => 0})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-7, #{type => integer, minimum => -6})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(9, #{type => integer, minimum => 10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(11, #{type => integer, maximum => 10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(1, #{type => integer, maximum => 0})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(-9, #{type => integer, maximum => -10})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(1, #{type => integer, minimum => 2, maximum => 4})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(5, #{type => integer, minimum => 2, maximum => 4})),
    % Not divisible by the number
    ?assertMatch({error, {not_valid, [], not_divisible}},
                 matcher:validate(-7, #{type => integer, divisible_by => 4})),
    % Not divisible by the number
    ?assertMatch({error, {not_valid, [], not_divisible}},
                 matcher:validate(9, #{type => integer, divisible_by => 4})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(15, #{type => integer, divisible_by => 5, minimum => 24, maximum => 29})),
    ok.

test_integer_bad_descriptor(_Config) ->
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(5, #{type => integer, minimum => null})),
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(5, #{type => integer, minimum => 4.0})),
    % The maximum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_maximum}},
                 matcher:validate(5, #{type => integer, maximum => null})),
    % The maximum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_maximum}},
                 matcher:validate(5, #{type => integer, maximum => 4.0})),
    % The divider must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_divisible_by}},
                 matcher:validate(5, #{type => integer, divisible_by => "4"})),
    % The divider must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_divisible_by}},
                 matcher:validate(5, #{type => integer, divisible_by => 4.0})),
    % The divider must not be zero
    ?assertMatch({error, {bad_descriptor, [], invalid_divisible_by}},
                 matcher:validate(5, #{type => integer, divisible_by => 0})),
    % Unknown format
    ?assertMatch({error, {bad_descriptor, [], unknown_format}},
                 matcher:validate(5, #{type => integer, format => xxx})),
    ok.

test_integer_int8(_Config) ->
    ?assertMatch({ok, -128},
                 matcher:validate(-128, #{type => integer, format => int8})),
    ?assertMatch({ok, 127},
                 matcher:validate(127, #{type => integer, format => int8})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-129, #{type => integer, format => int8})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(128, #{type => integer, format => int8})),
    ok.

test_integer_int16(_Config) ->
    ?assertMatch({ok, -32768},
                 matcher:validate(-32768, #{type => integer, format => int16})),
    ?assertMatch({ok, 32767},
                 matcher:validate(32767, #{type => integer, format => int16})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-32769, #{type => integer, format => int16})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(32768, #{type => integer, format => int16})),
    ok.

test_integer_int32(_Config) ->
    ?assertMatch({ok, -2147483648},
                 matcher:validate(-2147483648, #{type => integer, format => int32})),
    ?assertMatch({ok, 2147483647},
                 matcher:validate(2147483647, #{type => integer, format => int32})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-2147483649, #{type => integer, format => int32})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(2147483648, #{type => integer, format => int32})),
    ok.
test_integer_int64(_Config) ->
    ?assertMatch({ok, -9223372036854775808},
                 matcher:validate(-9223372036854775808, #{type => integer, format => int64})),
    ?assertMatch({ok, 9223372036854775807},
                 matcher:validate(9223372036854775807, #{type => integer, format => int64})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-9223372036854775809, #{type => integer, format => int64})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(9223372036854775808, #{type => integer, format => int64})),
    ok.

test_integer_uint8(_Config) ->
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, format => uint8})),
    ?assertMatch({ok, 255},
                 matcher:validate(255, #{type => integer, format => uint8})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-1, #{type => integer, format => uint8})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(256, #{type => integer, format => uint8})),
    ok.

test_integer_uint16(_Config) ->
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, format => uint16})),
    ?assertMatch({ok, 65535},
                 matcher:validate(65535, #{type => integer, format => uint16})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-1, #{type => integer, format => uint16})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(65536, #{type => integer, format => uint16})),
    ok.

test_integer_uint32(_Config) ->
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, format => uint32})),
    ?assertMatch({ok, 4294967295},
                 matcher:validate(4294967295, #{type => integer, format => uint32})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-1, #{type => integer, format => uint32})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(4294967296, #{type => integer, format => uint32})),
    ok.

test_integer_uint64(_Config) ->
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => integer, format => uint64})),
    ?assertMatch({ok, 18446744073709551615},
                 matcher:validate(18446744073709551615, #{type => integer, format => uint64})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(-1, #{type => integer, format => uint64})),
    % Out of range
    ?assertMatch({error, {not_valid, [], incorrect_format}},
                 matcher:validate(18446744073709551616, #{type => integer, format => uint64})),
    ok.

test_number_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => number, nullable => true})),
    ?assertMatch({ok, 12345},
                 matcher:validate(12345, #{type => number, nullable => false})),
    ?assertMatch({ok, 12345},
                 matcher:validate(12345, #{type => number})),
    ?assertMatch({ok, 12345.678},
                 matcher:validate(12345.678, #{type => number})),
    ?assertMatch({ok, 123456789012345678901234567890.123456789},
                 matcher:validate(123456789012345678901234567890.123456789, #{type => number})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => number})),
    ?assertMatch({ok, 0.0},
                 matcher:validate(0.0, #{type => number})),
    ?assertMatch({ok, -12345},
                 matcher:validate(-12345, #{type => number})),
    ?assertMatch({ok, -12345.678},
                 matcher:validate(-12345.678, #{type => number})),
    ?assertMatch({ok, -123456789012345678901234567890},
                 matcher:validate(-123456789012345678901234567890, #{type => number})),
    ?assertMatch({ok, -123456789012345678901234567890.123456789},
                 matcher:validate(-123456789012345678901234567890.123456789, #{type => number})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => number, minimum => 0})),
    ?assertMatch({ok, 0.0},
                 matcher:validate(0.0, #{type => number, minimum => 0})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => number, minimum => 0.0})),
    ?assertMatch({ok, 0.0},
                 matcher:validate(0.0, #{type => number, minimum => 0.0})),
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => number, minimum => 0})),
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => number, minimum => 0, exclusive_minimum => true})),
    ?assertMatch({ok, 0.00000000001},
                 matcher:validate(0.00000000001, #{type => number, minimum => 0, exclusive_minimum => true})),
    ?assertMatch({ok, 10},
                 matcher:validate(10, #{type => number, maximum => 10})),
    ?assertMatch({ok, 10.0},
                 matcher:validate(10.0, #{type => number, maximum => 10})),
    ?assertMatch({ok, 10},
                 matcher:validate(10, #{type => number, maximum => 10.0})),
    ?assertMatch({ok, 10.0},
                 matcher:validate(10.0, #{type => number, maximum => 10.0})),
    ?assertMatch({ok, 99},
                 matcher:validate(99, #{type => number, maximum => 100, exclusive_maximum => true})),
    ?assertMatch({ok, 99.9999999999},
                 matcher:validate(99.9999999999, #{type => number, maximum => 100, exclusive_maximum => true})),
    ?assertMatch({ok, -2},
                 matcher:validate(-2, #{type => number, minimum => -2.0, maximum => 4.0})),
    ?assertMatch({ok, -2.0},
                 matcher:validate(-2.0, #{type => number, minimum => -2.0, maximum => 4.0})),
    ?assertMatch({ok, 4},
                 matcher:validate(4, #{type => number, minimum => -2.0, maximum => 4.0})),
    ?assertMatch({ok, 4.0},
                 matcher:validate(4.0, #{type => number, minimum => -2.0, maximum => 4.0})),
    ?assertMatch({ok, 0},
                 matcher:validate(0, #{type => number, minimum => -2.0, maximum => 4.0})),
    ?assertMatch({ok, 0.0},
                 matcher:validate(0.0, #{type => number, minimum => -2.0, maximum => 4.0})),
    ok.

test_number_negative(_Config) ->
    % Mast be integer type
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("12345", #{type => number})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-1, #{type => number, minimum => 0})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-0.00000000000001, #{type => number, minimum => 0})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-7, #{type => number, minimum => -6})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(-6.00000000000001, #{type => number, minimum => -6})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(9, #{type => number, minimum => 10})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(9.9999999999999, #{type => number, minimum => 10})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(10, #{type => number, minimum => 10, exclusive_minimum => true})),
     % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(10.0, #{type => number, minimum => 10, exclusive_minimum => true})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(11, #{type => number, maximum => 10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(10.00000000000001, #{type => number, maximum => 10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(1, #{type => number, maximum => 0})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(0.00000000000001, #{type => number, maximum => 0})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(-9, #{type => number, maximum => -10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(-9.99999999999999, #{type => number, maximum => -10})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(-10, #{type => number, maximum => -10, exclusive_maximum => true})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(-10.0, #{type => number, maximum => -10, exclusive_maximum => true})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(1.99999999999999, #{type => number, minimum => 2, maximum => 4})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(4.00000000000001, #{type => number, minimum => 2, maximum => 4})),
    ok.

test_number_bad_descriptor(_Config) ->
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(5, #{type => number, minimum => null})),
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(5, #{type => number, minimum => "4.0"})),
    % The minimum must be boolean
    ?assertMatch({error, {bad_descriptor, [], invalid_exclusive_minimum}},
                 matcher:validate(5, #{type => number, minimum => 4.0, exclusive_minimum => 4.0})),
    % The minimum must be boolean
    ?assertMatch({error, {bad_descriptor, [], invalid_exclusive_minimum}},
                 matcher:validate(5, #{type => number, exclusive_minimum => 4.0})),
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor,[], invalid_maximum}},
                 matcher:validate(5, #{type => number, maximum => null})),
    % The minimum must be integer
    ?assertMatch({error, {bad_descriptor,[], invalid_maximum}},
                 matcher:validate(5, #{type => number, maximum => "4.0"})),
    % The maximum must be boolean
    ?assertMatch({error, {bad_descriptor,[], invalid_exclusive_maximum}},
                 matcher:validate(5, #{type => number, maximum => 4.0, exclusive_maximum => 4.0})),
    % The maximum must be boolean
    ?assertMatch({error, {bad_descriptor,[], invalid_exclusive_maximum}},
                 matcher:validate(5, #{type => number, exclusive_maximum => 4.0})),
    ok.


test_decimal_binary_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => decimal_binary, nullable => true})),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => decimal_binary, nullable => false})),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => decimal_binary})),
    ?assertMatch({ok, <<"-12345">>},
                 matcher:validate(<<"-12345">>, #{type => decimal_binary})),
    ?assertMatch({ok, <<"12345.678">>},
                 matcher:validate(<<"12345.678">>, #{type => decimal_binary})),
    ?assertMatch({ok, <<"-12345.678">>},
                 matcher:validate(<<"-12345.678">>, #{type => decimal_binary})),
    ?assertMatch({ok, <<"-5.33">>},
                 matcher:validate(<<"-5.33">>, #{type => decimal_binary, minimum => <<"-5.33">>})),
    ?assertMatch({ok, <<"6.22">>},
                 matcher:validate(<<"6.22">>, #{type => decimal_binary, minimum => <<"-5.33">>})),
    ?assertMatch({ok, <<"10.567">>},
                 matcher:validate(<<"10.567">>, #{type => decimal_binary, maximum => <<"10.567">>})),
    ?assertMatch({ok, <<"-100.45">>},
                 matcher:validate(<<"-100.45">>, #{type => decimal_binary, maximum => <<"10.567">>})),
    ?assertMatch({ok, <<"3.0">>},
                 matcher:validate(<<"3.0">>, #{type => decimal_binary, minimum => <<"2.1">>, maximum => <<"4.567">>})),
    ?assertMatch({ok, <<"2.1">>},
                 matcher:validate(<<"2.1">>, #{type => decimal_binary, minimum => <<"2.1">>, maximum => <<"4.567">>})),
    ?assertMatch({ok, <<"4.567">>},
                 matcher:validate(<<"4.567">>, #{type => decimal_binary, minimum => <<"2.1">>, maximum => <<"4.567">>})),
    ?assertMatch({ok, <<"12.2456">>},
                 matcher:validate(<<"12.2456">>, #{type => decimal_binary, precision => 4})),
    ?assertMatch({ok, <<"12.245">>},
                 matcher:validate(<<"12.245">>, #{type => decimal_binary, precision => 4})),
    ?assertMatch({ok, <<"12">>},
                 matcher:validate(<<"12">>, #{type => decimal_binary, precision => 4})),
    ?assertMatch({ok, <<"1200">>},
                 matcher:validate(<<"1200">>, #{type => decimal_binary, precision => -2})),
    ?assertMatch({ok, <<"4000">>},
                 matcher:validate(<<"4000">>, #{type => decimal_binary, precision => -2})),
    ok.

test_decimal_binary_negative(_Config) ->
    % Must be binary
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => decimal_binary})),
    % Must be binary
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate("12345", #{type => decimal_binary})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(<<"0.119">>, #{type => decimal_binary, minimum => <<"0.12">>})),
    % Less than minimum
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(<<"-6.31">>, #{type => decimal_binary, minimum => <<"-6.3">>})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<"10.1234">>, #{type => decimal_binary, maximum => <<"10.123">>})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<"0.01">>, #{type => decimal_binary, maximum => <<"0">>})),
    % Bigger than maximim
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<"-10.49">>, #{type => decimal_binary, maximum => <<"-10.5">>})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(<<"2.22">>, #{type => decimal_binary, minimum => <<"2.23">>, maximum => <<"4.56">>})),
    % Out of range
    ?assertMatch({error, {not_valid, [], too_big}},
                 matcher:validate(<<"4.561">>, #{type => decimal_binary, minimum => <<"2.23">>, maximum => <<"4.56">>})),
    % Too much precision
    ?assertMatch({error, {not_valid, [], incorrect_precision}},
                 matcher:validate(<<"-3.45678">>, #{type => decimal_binary, precision => 4})),
    % Too much precision
    ?assertMatch({error, {not_valid, [], incorrect_precision}},
                 matcher:validate(<<"3.45678">>, #{type => decimal_binary, precision => 4})),
    ok.

test_decimal_binary_bad_descriptor(_Config) ->
    % The minimum must be binary
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, minimum => null})),
    % The minimum must be binary
    ?assertMatch({error, {bad_descriptor, [], invalid_minimum}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, minimum => 4.0})),
    % The maximum must be binary
    ?assertMatch({error, {bad_descriptor, [], invalid_maximum}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, maximum => null})),
    % The maximum must be binary
    ?assertMatch({error, {bad_descriptor, [], invalid_maximum}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, maximum => 4.0})),
    % The precision must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_precision}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, precision => "4"})),
    % The precision must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_precision}},
                 matcher:validate(<<"5">>, #{type => decimal_binary, precision => 4.0})),
    ok.

test_boolean_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => boolean, nullable => true})),
    ?assertMatch({ok, true},
                 matcher:validate(true, #{type => boolean, nullable => false})),
    ?assertMatch({ok, true},
                 matcher:validate(true, #{type => boolean})),
    ?assertMatch({ok, false},
                 matcher:validate(false, #{type => boolean})),
    ok.

test_boolean_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => boolean})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(<<"2020:12:31">>, #{type => boolean})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(1999, #{type => boolean})),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(flase, #{type => boolean})),
    ok.

test_boolean_bad_descriptor(_Config) ->
    ?assertMatch({error, {bad_descriptor, [], invalid_nullable}},
                 matcher:validate(true, #{type => boolean, nullable => yes})), % The nullable must be true or false
    ok.

test_term_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => term, nullable => true})),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => term, nullable => false})),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => term})),
    ?assertMatch({ok, some_atom},
                 matcher:validate(some_atom, #{type => term})),
    ?assertMatch({ok, #{key := "value"}},
                 matcher:validate(#{key => "value"}, #{type => term})),
    ?assertMatch({ok, <<"-12345">>},
                 matcher:validate(<<"-12345">>, #{type => term, validator => fun erlang:is_binary/1})),
    ?assertMatch({ok, [1, 2, 3]},
                 matcher:validate([1, 2, 3], #{type => term, validator => fun erlang:is_list/1})),
    ?assertMatch({ok, <<"2020-12-31">>},
                 matcher:validate(<<"2020-12-31">>, #{type => term, validator => fun matcher_lib:is_date/1})),
    ok.

test_term_negative(_Config) ->
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(12345.0, #{type => term, validator => fun erlang:is_integer/1})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(<<"2020:12:31">>, #{type => term, validator => fun matcher_lib:is_date/1})),
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(1999, #{type => term, validator => fun matcher_lib:is_date/1})),
    ok.

test_term_bad_descriptor(_Config) ->
    % The validator must be a fun/1
    ?assertMatch({error, {bad_descriptor, [], invalid_validator}},
                 matcher:validate(<<"5">>, #{type => term, validator => null})),
    % The validator must be a fun/1
    ?assertMatch({error, {bad_descriptor, [], invalid_validator}},
                 matcher:validate(<<"5">>, #{type => term, validator => fun (_, _) -> true end})),
    ok.

test_custom_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => custom, name => <<"some_type">>, nullable => true})),
    ExtraTypes = #{<<"some_type">> => #{type => decimal_binary}},
    Options = #{extra_custom_types => ExtraTypes},
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => custom, name => <<"some_type">>, nullable => false}, Options)),
    ?assertMatch({ok, <<"12345">>},
                 matcher:validate(<<"12345">>, #{type => custom, name => <<"some_type">>, custom_types => ExtraTypes})),
    ok.

test_custom_negative(_Config) ->
    ExtraTypes = #{<<"some_type">> => #{type => decimal_binary}},
    Options = #{extra_custom_types => ExtraTypes},
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => custom, name => <<"some_type">>}, Options)),
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => custom, name => <<"some_type">>, custom_types => ExtraTypes})),
    ok.

test_custom_bad_descriptor(_Config) ->
    % No such type
    ?assertMatch({error, {bad_descriptor, [], invalid_custom_types}},
                  matcher:validate(12345.0, #{type => custom, name => <<"some_type">>, custom_types => #{}})),
    % The extra_types must be a map
    ?assertMatch({error, {bad_descriptor, [], invalid_custom_types}},
                  matcher:validate(12345.0, #{type => custom, name => <<"some_type">>, custom_types => []})),
    % No such type
    ?assertMatch({error, {bad_descriptor, [], invalid_custom_types}},
                  matcher:validate(12345.0, #{type => custom, name => <<"some_type">>}, #{})),
    ok.

test_tuple_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => tuple, nullable => true})),
    ?assertMatch({ok, {}},
                 matcher:validate({}, #{type => tuple, nullable => false})),
    ?assertMatch({ok, {}},
                 matcher:validate({}, #{type => tuple})),
    ?assertMatch({ok, {1}},
                 matcher:validate({1}, #{type => tuple})),
    ?assertMatch({ok, {1, a}},
                 matcher:validate({1, a}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]})),
    ?assertMatch({ok, {}},
                 matcher:validate({}, #{type => tuple, schema => []})),
    ok.

test_tuple_negative(_Config) ->
    % The value must be a tuple
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => tuple, nullable => false})),
    % The tuple size is not equal the schema size
    ?assertMatch({error, {not_valid, [], wrong_size}},
                 matcher:validate({1}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]})),
    % The second element is invalid
    ?assertMatch({error, {not_valid, [{te, 2}], not_match}},
                 matcher:validate({1, b}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]})),
    ok.

test_tuple_bad_descriptor(_Config) ->
     % The schema must be a list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                  matcher:validate({}, #{type => tuple, schema => #{}})),
    % There is an invalid descriptor in the schema 
    ?assertMatch({error, {bad_descriptor, [{te, 1}], no_type}},
                  matcher:validate({1}, #{type => tuple, schema => [#{}]})),
    ok.

test_list_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => list, nullable => true})),
    ?assertMatch({ok, []},
                 matcher:validate([], #{type => list, nullable => false})),
    ?assertMatch({ok, []},
                 matcher:validate([], #{type => list})),
    ?assertMatch({ok, []},
                 matcher:validate([], #{type => list, schema => []})),
    ?assertMatch({ok, [1]},
                 matcher:validate([1], #{type => list})),
    ?assertMatch({ok, [a, b, 1, 2, #{}]},
                 matcher:validate([a, b, 1, 2, #{}], #{type => list})),
    ?assertMatch({ok, [1, a]},
                 matcher:validate([1, a], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]})),
    ?assertMatch({ok, [1, a, a]},
                 matcher:validate([1, a, a], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]})),
    ?assertMatch({ok, []},
                 matcher:validate([], #{type => list, min_length => 0})),
    ?assertMatch({ok, []},
                 matcher:validate([], #{type => list, max_length => 0})),
    ?assertMatch({ok, [a]},
                 matcher:validate([a], #{type => list, max_length => 1})),
    ?assertMatch({ok, [a]},
                 matcher:validate([a], #{type => list, min_length => 1, max_length => 1})),
    ?assertMatch({ok, [a, "b", <<"c">>]},
                 matcher:validate([a, "b", <<"c">>], #{type => list, min_length => 2, max_length => 3})),
    ?assertMatch({ok, [a, "b", <<"c">>]},
                 matcher:validate([a, "b", <<"c">>], #{type => list, min_length => 2, max_length => 3,
                                                       schema => [#{type => term}]})),
    ok.

test_list_negative(_Config) ->
    % The value must be a list
    ?assertMatch({error, {not_valid, [], wrong_type}},
                 matcher:validate(12345.0, #{type => list, nullable => false})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_short}},
                 matcher:validate([], #{type => list, min_length => 1, schema => [#{type => integer}]})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_short}},
                 matcher:validate([1, 2], #{type => list, min_length => 3, schema => [#{type => integer}]})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_long}},
                 matcher:validate([2], #{type => list, max_length => 0, schema => [#{type => integer}]})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_long}},
                 matcher:validate([2, 1, 0], #{type => list, max_length => 2, schema => [#{type => integer}]})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_short}},
                 matcher:validate([2], #{type => list, min_length => 2, max_length => 3, schema => [#{type => integer}]})),
    % Wrong length
    ?assertMatch({error, {not_valid, [], too_long}},
                 matcher:validate([2, 0, 5, 2], #{type => list, min_length => 2, max_length => 3, schema => [#{type => integer}]})),
    % The list item is invalid
    ?assertMatch({error, {not_valid, [{li, 1}], not_match}},
                 matcher:validate([1], #{type => list, schema => [#{type => const, value => a}]})),
    % The second item is invalid
    ?assertMatch({error, {not_valid, [{li, 2}], not_match}},
                 matcher:validate([1, b], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]})),
    ok.

test_list_bad_descriptor(_Config) ->
    % The min_length must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_min_length}}, matcher:validate([], #{type => list, min_length => null})),
    % The min_length must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_min_length}}, matcher:validate([], #{type => list, min_length => 1.0})),
    % The min_length must be >= 0
    ?assertMatch({error, {bad_descriptor, [], invalid_min_length}}, matcher:validate([], #{type => list, min_length => -1})),
    % The max_length must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_max_length}}, matcher:validate([], #{type => list, max_length => null})),
    % The max_length must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_max_length}}, matcher:validate([], #{type => list, max_length => 1.0})),
    % The max_length must be >= 0
    ?assertMatch({error, {bad_descriptor, [], invalid_max_length}}, matcher:validate([], #{type => list, max_length => -1})),
    % The schema must be a list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}}, matcher:validate([], #{type => list, schema => #{}})),
    % There is an invalid descriptor in the schema 
    ?assertMatch({error, {bad_descriptor, [{li, 1}], no_type}}, matcher:validate([1], #{type => list, schema => [#{}]})),
    ok.

test_map_positive(_Config) ->
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => map, nullable => true})),
    ?assertMatch({ok, #{}},
                 matcher:validate(#{}, #{type => map, nullable => false})),
    % Check validation without a schema
    ?assertMatch({ok, #{}},
                 matcher:validate(#{}, #{type => map})),
    ?assertMatch({ok, #{some_key := "some_value"}},
                 matcher:validate(#{some_key => "some_value"}, #{type => map})),
    % Check simple schema
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, schema => #{key1 => #{}, <<"key2">> => #{}}})),
    % Check optional without the key
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, schema => #{key1 => #{}, <<"key2">> => #{}, "key3" => #{optional => true}}})),
    % Check optional with the key
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true, "key3" := 1}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, schema => #{key1 => #{}, <<"key2">> => #{}, "key3" => #{optional => true}}})),
    % Check default option
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := 4}},
                 matcher:validate(#{key1 => "some_value"},
                                  #{type => map, schema =>
                                        #{key1 => #{descriptor => #{type => list}},
                                          <<"key2">> => #{optional => true, default => 4, descriptor => #{type => term}}
                                         }})),
    % Check a schema with descriptors
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, schema =>
                                        #{key1 => #{descriptor => #{type => list}},
                                          <<"key2">> => #{descriptor => #{type => boolean}}}})),
    % Check requires and conflicts options
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, schema =>
                                        #{key1 => #{requires => [<<"key2">>], descriptor => #{type => list}},
                                          <<"key2">> => #{conflicts => ["key3"], descriptor => #{type => boolean}}}})),
    % Check validation of nested maps
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true, "key3" := #{k1 := 1, k2 := 2}}},
                  matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => #{k1 => 1, k2 => 2}},
                                   #{type => map, schema =>
                                         #{key1 => #{requires => [<<"key2">>], descriptor => #{type => list}},
                                           <<"key2">> => #{descriptor => #{type => boolean}},
                                           "key3" => #{descriptor => #{type => map, schema =>
                                                                           #{k1 => #{descriptor => #{type => const, value => 1}},
                                                                             k2 => #{}}}}}})),
    % Check allow_extra_keys option without extra keys
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, allow_extra_keys => true,
                                    schema => #{key1 => #{}, <<"key2">> => #{}}})),
    % Check allow_extra_keys option with extra keys
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := true, "key3" := 1}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, allow_extra_keys => true,
                                    schema => #{key1 => #{}, <<"key2">> => #{}}})),
    % Check extra_keys_descriptor option with extra keys
    ?assertMatch({ok, #{key1 := "some_value", "key2" := true, "key3" := 1}},
                 matcher:validate(#{key1 => "some_value", "key2" => true, "key3" => 1},
                                  #{type => map, extra_keys_descriptor => #{type => list},
                                    schema => #{key1 => #{}}})),
    % Check extra_values_descriptor option with extra keys
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                                  #{type => map, extra_values_descriptor => #{type => integer},
                                    schema => #{key1 => #{}}})),
    % Check min_extra_keys option
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                                  #{type => map, min_extra_keys => 3})),
    % Check max_extra_keys option
    ?assertMatch({ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                                  #{type => map, max_extra_keys => 3})),
    ok.

test_map_negative(_Config) ->
    % The value must be a map
    ?assertMatch({error, {not_valid, [], wrong_type}}, matcher:validate(12345.0, #{type => map, nullable => false})),
    % There is no key1 in the map
    ?assertMatch({error, {not_valid, [{mk, key1}], key_missed}},
                 matcher:validate(#{}, #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}}})),
    % There is extra key key2 in the map
    ?assertMatch({error, {not_valid, [{mk, key2}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => 1, key2 => 2}, #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}}})),
    % Incorrect key value type
    ?assertMatch({error, {not_valid, [{mv, key1}], wrong_type}},
                 matcher:validate(#{key1 => true, key2 => 2},
                                  #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}, key2 => #{}}})),
    % There is no a required key
    ?assertMatch({error, {not_valid, [{mk, key1}], missing_required}},
                 matcher:validate(#{key1 => 1},
                                  #{type => map, schema => #{key1 => #{requires => [key2], descriptor => #{type => integer}},
                                                             key2 => #{optional => true}}})),
    % There is a confict key
    ?assertMatch({error, {not_valid, [{mk, key1}], conflicts}},
                 matcher:validate(#{key1 => 1, key2 => 1},
                                  #{type => map, schema => #{key1 => #{conflicts => [key2], descriptor => #{type => integer}},
                                                             key2 => #{optional => true}}})),
    % Check allow_extra_keys option
    ?assertMatch({error, {not_valid, [{mk, "key3"}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, allow_extra_keys => false,
                                    schema => #{key1 => #{}, <<"key2">> => #{}}})),
    % Check extra_keys_descriptor
    ?assertMatch({error, {not_valid, [{mk, <<"key2">>}], wrong_type}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, extra_keys_descriptor => #{type => list},
                                    schema => #{key1 => #{}}})),
    % Check extra_keys_descriptor with allow_extra_keys = false
    ?assertMatch({error, {not_valid, [{mk, <<"key2">>}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                                  #{type => map, allow_extra_keys => false, extra_keys_descriptor => #{type => list},
                                    schema => #{key1 => #{}}})),
    % Check extra_valuse_descriptor
    ?assertMatch({error, {not_valid, [{mv, <<"key2">>}], wrong_type}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, extra_values_descriptor => #{type => integer},
                                    schema => #{key1 => #{}}})),
    % Check extra_values_descriptor with allow_extra_keys = false
    ?assertMatch({error, {not_valid, [{mk, <<"key2">>}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 1},
                                  #{type => map, allow_extra_keys => false, extra_values_descriptor => #{type => integer},
                                    schema => #{key1 => #{}}})),
    % Check min_extra_keys option 
    ?assertMatch({error, {not_valid, [], not_enough_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4},
                                  #{type => map, min_extra_keys => 3})),
    % Check min_extra_keys option  with allow_extra_keys = false
    ?assertMatch({error, {not_valid, [{mk, key1}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => "some_value"},
                                  #{type => map, min_extra_keys => 3, allow_extra_keys => false})),
    % Check max_extra_keys option 
    ?assertMatch({error, {not_valid, [], too_much_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 1},
                                  #{type => map, max_extra_keys => 2})),
    % Check max_extra_keys option  with allow_extra_keys = false
    ?assertMatch({error, {not_valid, [{mk, key1}], extra_keys_not_allowed}},
                 matcher:validate(#{key1 => "some_value"},
                                  #{type => map, max_extra_keys => 3, allow_extra_keys => false})),
    ok.

test_map_bad_descriptor(_Config) ->
    % The schema must be a map
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}}, matcher:validate(#{}, #{type => map, schema => []})),
    % The schema must be a map
    ?assertMatch({error, {bad_descriptor, [], invalid_allow_extra_keys}},
                 matcher:validate(#{}, #{type => map, allow_extra_keys => 1})),
    % The optional option value must be boolean
    ?assertMatch({error, {bad_descriptor, [{mk, key1}], invalid_optional}},
                 matcher:validate(#{key1 => 1}, #{type => map, schema => #{key1 => #{optional => 1}}})),
    % The descriptor is invalid
    ?assertMatch({error, {bad_descriptor, [{mv, key1}], invalid_descriptor}},
                 matcher:validate(#{key1 => 1}, #{type => map, schema => #{key1 => #{descriptor => 1}}})),
    % The type of the key is unsupported
    ?assertMatch({error, {bad_descriptor, [{mv, key1}], unsupported_type}},
                 matcher:validate(#{key1 => 1}, #{type => map, schema => #{key1 => #{descriptor => #{type => xxx}}}})),
    % The requires key must be list
    ?assertMatch({error, {bad_descriptor, [{mk, key1}], invalid_requires}},
                 matcher:validate(#{key1 => 1, key2 => 2}, #{type => map, schema => #{key1 => #{requires => key2},
                                                                                      key2 => #{}}})),
    % The requires key must be list
    ?assertMatch({error, {bad_descriptor, [{mk, key1}], invalid_conflicts}},
                 matcher:validate(#{key1 => 1, key2 => 2}, #{type => map, schema => #{key1 => #{conflicts => key2},
                                                                                      key2 => #{}}})),
    % The allow_extra_keys option must be boolean
    ?assertMatch({error, {bad_descriptor, [], invalid_allow_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, allow_extra_keys => 1, schema => #{key1 => #{}, <<"key2">> => #{}}})),
    % The extra_keys_descriptor option must be descriptor() type
    ?assertMatch({error, {bad_descriptor, [], invalid_extra_keys_descriptor}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, extra_keys_descriptor => true, schema => #{key1 => #{}}})),
    % The extra_values_descriptor option must be descriptor() type
    ?assertMatch({error, {bad_descriptor, [], invalid_extra_values_descriptor}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                                  #{type => map, extra_values_descriptor => true, schema => #{key1 => #{}}})),
    % min_extra_keys option must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_min_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4}, #{type => map, min_extra_keys => true})),
    % min_extra_keys option must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_min_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4}, #{type => map, min_extra_keys => -1})),
    % max_extra_keys option must be integer
    ?assertMatch({error, {bad_descriptor, [], invalid_max_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4}, #{type => map, max_extra_keys => true})),
    % max_extra_keys option must be non negative
    ?assertMatch({error, {bad_descriptor, [], invalid_max_extra_keys}},
                 matcher:validate(#{key1 => "some_value", <<"key2">> => 4}, #{type => map, max_extra_keys => -1})),
    ok.


test_alt_positive(_Config) ->
    % Check null value with nullable = true
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => alt, schema => [{#{type => integer}, #{type => integer}}], nullable => true})),
    % Check a valid value with nullable = false
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => alt, schema => [{#{type => integer}, #{type => integer}}], nullable => false})),
    % Check a simple schema with two alternatives
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => alt, schema => [{#{type => integer}, #{type => integer}},
                                                               {#{type => const, value => a}, #{type => term}}]})),
    ?assertMatch({ok, a},
                 matcher:validate(a, #{type => alt, schema => [{#{type => integer}, #{type => integer}},
                                                               {#{type => const, value => a}, #{type => term}}]})),
    ok.

test_alt_negative(_Config) ->
    % Both pre-descriptors failed
    ?assertMatch({error, {not_valid, [], alternatives_not_match}},
                 matcher:validate(1.0, #{type => alt, schema => [{#{type => integer}, #{type => integer, minimum => 10}},
                                                                 {#{type => const, value => a}, #{type => integer}}]})),
    % The first pre-descriptor successed but the essential descriptor failed
    ?assertMatch({error, {not_valid, [], too_small}},
                 matcher:validate(5, #{type => alt, schema => [{#{type => integer}, #{type => integer, minimum => 10}},
                                                               {#{type => const, value => a}, #{type => integer}}]})),
    ok.

test_alt_bad_descriptor(_Config) ->
     % The schema must be a list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                  matcher:validate(1, #{type => alt, schema => #{}})),
     % The schema must be a non-empty list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                  matcher:validate(1, #{type => alt, schema => []})),
    % There is an invalid schema (must consists of two-elements tuple)
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                  matcher:validate(1, #{type => alt, schema => [#{}]})),
    % There is an invalid pre-descriptor
    ?assertMatch({error, {bad_descriptor, [], no_type}},
                  matcher:validate(1, #{type => alt, schema => [{#{}, #{type => term}}]})),
    % There is an invalid descriptor
    ?assertMatch({error, {bad_descriptor, [], no_type}},
                  matcher:validate(1, #{type => alt, schema => [{#{type => term}, #{}}]})),
    ok.


test_alt_simple_positive(_Config) ->
    % Check null value with nullable = true
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => alt_simple, schema => [#{type => integer}], nullable => true})),
    % Check a valid value with nullable = false
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => alt_simple, schema => [#{type => integer}], nullable => false})),
    % Check a simple schema with two alternatives
    ?assertMatch({ok, 1},
                 matcher:validate(1, #{type => alt_simple, schema => [#{type => integer},
                                                                      #{type => const, value => a}]})),
    ?assertMatch({ok, a},
                 matcher:validate(a, #{type => alt_simple, schema => [#{type => integer},
                                                                      #{type => const, value => a}]})),
    ok.

test_alt_simple_negative(_Config) ->
    % Both descriptors failed
    ?assertMatch({error, {not_valid, [], alternatives_not_match}},
                 matcher:validate(1.0, #{type => alt_simple, schema => [#{type => integer},
                                                                        #{type => const, value => a}]})),
    % Both descriptors failed
    ?assertMatch({error, {not_valid, [], alternatives_not_match}},
                 matcher:validate(5, #{type => alt_simple, schema => [#{type => integer, minimum => 10},
                                                                      #{type => const, value => a}]})),
    ok.

test_alt_simple_bad_descriptor(_Config) ->
    % The schema must be a list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                 matcher:validate(1, #{type => alt_simple, schema => #{}})),
    % The schema must be a non-empty list
    ?assertMatch({error, {bad_descriptor, [], invalid_schema}},
                 matcher:validate(1, #{type => alt_simple, schema => []})),
    % There is an invalid descriptor in the schema
    ?assertMatch({error, {bad_descriptor, [], no_type}},
                 matcher:validate(1, #{type => alt_simple, schema => [#{}]})),
    ok.


test_converters_positive(_Config) ->
    % Check null value with nullable = true
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, pre_converter => fun integer_to_binary/1, nullable => true})),
    ?assertMatch({ok, null},
                 matcher:validate(null, #{type => utf8_binary, post_converter => fun binary_to_integer/1, nullable => true})),
    % Check pre_converter
    ?assertMatch({ok, <<"1">>},
                 matcher:validate(1, #{type => utf8_binary, pre_converter => fun integer_to_binary/1})),
    % Check post_converter
    ?assertMatch({ok, <<"1">>},
                 matcher:validate(1, #{type => integer, post_converter => fun integer_to_binary/1})),
    % Check both converters work togather
    ?assertMatch({ok, "1"},
                 matcher:validate(1, #{type => utf8_binary, pre_converter => fun integer_to_binary/1, post_converter => fun binary_to_list/1})),
    ok.

test_converters_negative(_Config) ->
    % The pre-converter converts to a wrong type
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(1, #{type => integer, pre_converter => fun list_to_float/1})),
    % The post-converter failed to convert
    ?assertMatch({error, {not_valid, [], not_match}},
                 matcher:validate(true, #{type => boolean, post_converter => fun list_to_binary/1})),
    ok.


test_converters_bad_descriptor(_Config) ->
    % The value of the nullable option is incorrect
    ?assertMatch({error, {bad_descriptor, [], invalid_nullable}},
                 matcher:validate(null, #{type => utf8_binary, pre_converter => fun integer_to_binary/1, nullable => 1})),
    % A value of the pre-converter must be a function with arity 1
    ?assertMatch({error, {bad_descriptor, [], invalid_pre_converter}},
                 matcher:validate(1, #{type => utf8_binary, pre_converter => fun integer_to_binary/2})),
    % A value of the post-converter must be a function with arity 1
    ?assertMatch({error, {bad_descriptor, [], invalid_post_converter}},
                 matcher:validate(<<"123">>, #{type => utf8_binary, post_converter => fun integer_to_binary/2})),
    ok.

