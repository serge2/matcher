%%%-------------------------------------------------------------------
%%% File    : matcher_lib_SUITE.erl
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : 
%%%
%%% Created : 
%%%-------------------------------------------------------------------
-module(matcher_lib_SUITE).

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
     test_is_date,
     test_is_iso8601_datetime,
     test_is_email,
     test_is_language_alpha2,
     test_is_language_alpha3,
     test_is_country_alpha2,
     test_is_country_alpha3,
     test_is_int8,
     test_is_int16,
     test_is_int32,
     test_is_int64,
     test_is_uint8,
     test_is_uint16,
     test_is_uint32,
     test_is_uint64
    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
test_is_date() -> 
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
test_is_date(_Config) ->
    ?assertMatch(true, matcher_lib:is_date(<<"1999-01-23">>)),
    ?assertMatch(false, matcher_lib:is_date(<<"999-01-23">>)), % A year must consist of 4 digits
    ?assertMatch(false, matcher_lib:is_date(<<"01999-01-23">>)), % A year must consist of 4 digits
    ?assertMatch(false, matcher_lib:is_date(<<"1999-1-23">>)), % A month must consist of 2 digits
    ?assertMatch(false, matcher_lib:is_date(<<"1999-112-23">>)), % A month must consist of 2 digits
    ?assertMatch(false, matcher_lib:is_date(<<"1999-10-3">>)), % A day must consist of 2 digits
    ?assertMatch(false, matcher_lib:is_date(<<"1999-10-321">>)), % A day must consist of 2 digits
    ?assertMatch(false, matcher_lib:is_date(<<"1999-00-23">>)), % A month must be in range 1-12
    ?assertMatch(false, matcher_lib:is_date(<<"1999-13-23">>)), % A month must be in range 1-12
    ?assertMatch(false, matcher_lib:is_date(<<"1999-01-00">>)), % A day must be in range 1-31
    ?assertMatch(false, matcher_lib:is_date(<<"1999-01-32">>)), % A day must be in range 1-31
    ?assertMatch(false, matcher_lib:is_date(<<"1999-02-30">>)), % There are 28(29) days in Feb
    ?assertError(_, matcher_lib:is_date("1999-01-23")), % Must be a binary
    ok.

test_is_iso8601_datetime(_Config) ->
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-01-31T23:59:59.765432Z">>)), % With microseconds
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-01-31T23:59:59.765432">>)),  % Without Zulu timezone
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-01-31T00:00:00.765Z">>)),    % With milliseconds
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-01-31T00:00:00.765">>)),     % Without Zulu timezone
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-12-31T23:00:00Z">>)),        % Integer seconds
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"1999-12-31T23:00:00">>)),         % Without Zulu timezone
    ?assertMatch(false, matcher_lib:is_iso8601_datetime(<<"999-12-31T23:00:00">>)),         % A year must be of four digits
    ?assertMatch(true, matcher_lib:is_iso8601_datetime(<<"0999-12-31T23:00:00">>)),         % A year must be of four digits
    ?assertError(_,  matcher_lib:is_iso8601_datetime("0999-12-31T23:00:00")),               % Must be a binary
    ok.

test_is_email(_Config) ->
    ?assertMatch(true, matcher_lib:is_email(<<"john.doe@some-domain.ua">>)),
    ?assertMatch(true, matcher_lib:is_email(<<"john0doe@some.do-main.org">>)),
    ?assertMatch(true, matcher_lib:is_email(<<"JOHN.DOE@SOME-DOMAIN.UA">>)),
    ?assertMatch(true, matcher_lib:is_email(<<"john.doe@s.ua">>)),
    ?assertMatch(false, matcher_lib:is_email(<<"john.doe@-.ua">>)),            % Subdomain cannot begin/end with "-"
    ?assertMatch(false, matcher_lib:is_email(<<"john.doe@ua">>)),              % At least one subdomain is needed
    ?assertMatch(false, matcher_lib:is_email(<<"john.doe@some-domain.u">>)),   % Lengtgh of a top domain must be 2 or more 
    ?assertMatch(false, matcher_lib:is_email(<<"john.doe@-some-domain.ua">>)), % Subdomain cannot begin/end with "-"
    ?assertMatch(false, matcher_lib:is_email(<<"john.doe@some-domain-.ua">>)), % Subdomain cannot begin/end with "-"
    ?assertError(_,  matcher_lib:is_email("john0doe@some.do-main.org")),  % Must be a binary
    ok.

test_is_language_alpha2(_Config) ->
    ?assertMatch(true, matcher_lib:is_language_alpha2(<<"uk">>)),
    ?assertMatch(true, matcher_lib:is_language_alpha2(<<"en">>)),
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"UK">>)),  % Must be lowercase
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"Uk">>)),  % Must be lowercase
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"ukr">>)), % Must be two symbols
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"u">>)),   % Must be two symbols
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"">>)),    % Must be two symbols
    ?assertMatch(false, matcher_lib:is_language_alpha2(<<"u1">>)),  % Only alpha symbols allowed
    ?assertError(_, matcher_lib:is_language_alpha2("uk")),          % Must be a binary
    ok.

test_is_language_alpha3(_Config) ->
    ?assertMatch(true, matcher_lib:is_language_alpha3(<<"ukr">>)),
    ?assertMatch(true, matcher_lib:is_language_alpha3(<<"eng">>)),
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"UKR">>)),  % Must be lowercase
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"Ukr">>)),  % Must be lowercase
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"ukra">>)), % Must be three symbols
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"uk">>)),   % Must be three symbols
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"">>)),     % Must be three symbols
    ?assertMatch(false, matcher_lib:is_language_alpha3(<<"uk1">>)),  % Only alpha symbols allowed
    ?assertError(_, matcher_lib:is_language_alpha3("ukr")),          % Must be a binary
    ok.

test_is_country_alpha2(_Config) ->
    ?assertMatch(true, matcher_lib:is_country_alpha2(<<"UA">>)),
    ?assertMatch(true, matcher_lib:is_country_alpha2(<<"UK">>)),
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"ua">>)),  % Must be uppercase
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"Ua">>)),  % Must be uppercase
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"UKR">>)), % Must be two symbols
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"U">>)),   % Must be two symbols
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"">>)),    % Must be two symbols
    ?assertMatch(false, matcher_lib:is_country_alpha2(<<"U1">>)),  % Only alpha symbols allowed
    ?assertError(_, matcher_lib:is_country_alpha2("UA")),          % Must be a binary
    ok.

test_is_country_alpha3(_Config) ->
    ?assertMatch(true, matcher_lib:is_country_alpha3(<<"UKR">>)),
    ?assertMatch(true, matcher_lib:is_country_alpha3(<<"GBT">>)),
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"ukr">>)),  % Must be uppercase
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"Ukr">>)),  % Must be uppercase
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"UKRA">>)), % Must be three symbols
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"UK">>)),   % Must be three symbols
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"">>)),     % Must be three symbols
    ?assertMatch(false, matcher_lib:is_country_alpha3(<<"UK1">>)),  % Only alpha symbols allowed
    ?assertError(_, matcher_lib:is_country_alpha3("UKR")),          % Must be a binary
    ok.

test_is_int8(_Config) ->
    ?assertMatch(true, matcher_lib:is_int8(0)),
    ?assertMatch(true, matcher_lib:is_int8(-128)),
    ?assertMatch(false, matcher_lib:is_int8(-129)),
    ?assertMatch(true, matcher_lib:is_int8(127)),
    ?assertMatch(false, matcher_lib:is_int8(128)),
    ?assertError(_, matcher_lib:is_int8(0.0)),          % Must be an integer
    ok.

test_is_int16(_Config) ->
    ?assertMatch(true, matcher_lib:is_int16(0)),
    ?assertMatch(true, matcher_lib:is_int16(-32768)),
    ?assertMatch(false, matcher_lib:is_int16(-32769)),
    ?assertMatch(true, matcher_lib:is_int16(32767)),
    ?assertMatch(false, matcher_lib:is_int16(32768)),
    ?assertError(_, matcher_lib:is_int16(0.0)),          % Must be an integer
    ok.

test_is_int32(_Config) ->
    ?assertMatch(true, matcher_lib:is_int32(0)),
    ?assertMatch(true, matcher_lib:is_int32(-2147483648)),
    ?assertMatch(false, matcher_lib:is_int32(-2147483649)),
    ?assertMatch(true, matcher_lib:is_int32(2147483647)),
    ?assertMatch(false, matcher_lib:is_int32(2147483648)),
    ?assertError(_, matcher_lib:is_int32(0.0)),             % Must be an integer
    ok.

test_is_int64(_Config) ->
    ?assertMatch(true, matcher_lib:is_int64(0)),
    ?assertMatch(true, matcher_lib:is_int64(-9223372036854775808)),
    ?assertMatch(false, matcher_lib:is_int64(-9223372036854775809)),
    ?assertMatch(true, matcher_lib:is_int64(9223372036854775807)),
    ?assertMatch(false, matcher_lib:is_int64(9223372036854775808)),
    ?assertError(_, matcher_lib:is_int64(0.0)),                      % Must be an integer
    ok.

test_is_uint8(_Config) ->
    ?assertMatch(true, matcher_lib:is_uint8(0)),
    ?assertMatch(false, matcher_lib:is_uint8(-1)),
    ?assertMatch(true, matcher_lib:is_uint8(255)),
    ?assertMatch(false, matcher_lib:is_uint8(256)),
    ?assertError(_, matcher_lib:is_uint8(0.0)),          % Must be an integer
    ok.

test_is_uint16(_Config) ->
    ?assertMatch(true, matcher_lib:is_uint16(0)),
    ?assertMatch(false, matcher_lib:is_uint16(-1)),
    ?assertMatch(true, matcher_lib:is_uint16(65535)),
    ?assertMatch(false, matcher_lib:is_uint16(65536)),
    ?assertError(_, matcher_lib:is_int16(0.0)),          % Must be an integer
    ok.

test_is_uint32(_Config) ->
    ?assertMatch(true, matcher_lib:is_uint32(0)),
    ?assertMatch(false, matcher_lib:is_uint32(-1)),
    ?assertMatch(true, matcher_lib:is_uint32(4294967295)),
    ?assertMatch(false, matcher_lib:is_uint32(4294967296)),
    ?assertError(_, matcher_lib:is_uint32(0.0)),             % Must be an integer
    ok.

test_is_uint64(_Config) ->
    ?assertMatch(true, matcher_lib:is_uint64(0)),
    ?assertMatch(false, matcher_lib:is_uint64(-1)),
    ?assertMatch(true, matcher_lib:is_uint64(18446744073709551615)),
    ?assertMatch(false, matcher_lib:is_uint64(18446744073709551616)),
    ?assertError(_, matcher_lib:is_uint64(0.0)),                      % Must be an integer
    ok.





