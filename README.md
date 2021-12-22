matcher
=====

This application proposes the API for validaton of arbitrary erlang terms against a specification (also it's called as descriptor).

API
---
The function does validation of a term using the specification.
```
matcher:validate(term(), matcher:descriptor()) -> {ok, term()} | {error, matcher:error()}.
```
The function behaves as the previous one but also uses extra options.
```
matcher:validate(term(), matcher:descriptor(), matcher:options()) -> {ok, term()} | {error, matcher:error()}.
```
The function behaves as the previous one but also uses custom types.
```
matcher:validate(term(), matcher:descriptor(), matcher:options(), matcher:matchers()) -> {ok, term()} | {error, matcher:error()}.
```
Using this function one can validate a specification.
```
matcher_descriptor:validate_descriptor(Descriptor :: term()) -> ok | {error, matcher:error()}.
```

Build
-----
    $ rebar3 compile

Examples
--------
### Constant
#### Positive cases
```
    > matcher:validate(1, #{type => const, nullable => false, value => 1})
    {ok, 1}

    > matcher:validate(null, #{type => const, nullable => true, value => 1}).
    {ok, null}

    > matcher:validate(some_atom, #{type => const, value => some_atom}).
    {ok, some_atom}
 
    > matcher:validate(10, #{type => const, value => 10}).
    {ok, 10}

    > matcher:validate(#{a => b}, #{type => const, value => #{a => b}}).
    {ok, #{a := b}}
```

#### Negative cases
```
    > matcher:validate(null, #{type => const, value => 1}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(null, #{type => const, nullable => false, value => 1}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(some_atom2, #{type => const, value => some_atom1}).
    {error, {not_valid, [], not_match}}
 
    > matcher:validate(10, #{type => const, value => 11}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(#{a => b, c => d}, #{type => const, value => #{a => b}}).
    {error, {not_valid, [], not_match}}
```

### Enum
#### Positive cases
```
    > matcher:validate(null, #{type => enum, nullable => true, values => [1, some_atom, #{}]}).
    {ok, null}

    > matcher:validate(some_atom, #{type => enum, nullable => false, values => [1, some_atom, #{}]}).
    {ok, some_atom}

    > matcher:validate(some_atom, #{type => enum, values => [1, some_atom, #{}]}).
    {ok, some_atom}

    > matcher:validate(1, #{type => enum, values => [1, some_atom, #{}]}).
    {ok, 1}

    > matcher:validate(#{}, #{type => enum, values => [1, some_atom, #{}]}).
    {ok, #{}}
```

#### Negative cases
```
    > matcher:validate(null, #{type => enum, values => [1, some_atom, #{}]}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(null, #{type => enum, nullable => false, values => [1, some_atom, #{}]}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(some_atom2, #{type => enum, values => [1, some_atom, #{}]}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(10, #{type => enum, values => [1, some_atom, #{}]}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(#{a => b}, #{type => enum, values => [1, some_atom, #{}]}).
    {error, {not_valid, [], not_match}}
```

### Binary
#### Positive cases
```
    > matcher:validate(null, #{type => binary, nullable => true}).
    {ok, null}

    > matcher:validate(<<1,2,3>>, #{type => binary, nullable => false}).
    {ok, <<1,2,3>>}

    > matcher:validate(<<>>, #{type => binary}).
    {ok, <<>>}

    > matcher:validate(<<1,2,3>>, #{type => binary}).
    {ok, <<1,2,3>>}

    > matcher:validate(<<1,2,3,4>>, #{type => binary, min_size => 3}).
    {ok, <<1,2,3,4>>}

    > matcher:validate(<<1,2>>, #{type => binary, max_size => 3}).
    {ok, <<1,2>>}
```

#### Negative cases
```
    > matcher:validate(null, #{type => binary}).
    {error, {not_valid, [], wrong_type}}
 
    > matcher:validate(null, #{type => binary, nullable => false}).
    {error, {not_valid, [], wrong_type}}
 
    > matcher:validate(some_atom2, #{type => binary}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(1, #{type => binary}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(<<1,2,3>>, #{type => binary, min_size => 4}).
    {error, {not_valid, [], too_small}}

    > matcher:validate(<<1,2,3>>, #{type => binary, max_size => 2}).
    {error, {not_valid, [], too_big}}
```

### UTF-8 binary
#### Positive cases
 ```
   > matcher:validate(null, #{type => utf8_binary, nullable => true}).
    {ok, null}

    > matcher:validate(<<"Hello">>, #{type => utf8_binary, nullable => false}).
    {ok, <<"Hello">>}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary}).
    {ok, <<"Привет"/utf8>>}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 6}).
    {ok, <<"Привет"/utf8>>}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 7}).
    {ok, <<"Привет"/utf8>>}

    > matcher:validate(<<"123456">>, #{type => utf8_binary, min_length => 6, pattern => <<"^[0-9]*">>}).
    {ok, <<"123456">>}

    > matcher:validate(<<"12345">>, #{type => utf8_binary, max_length => 5, pattern => <<"^[0-9]*">>}).
    {ok, <<"12345">>}

    > matcher:validate(<<"1976-01-08">>, #{type => utf8_binary, format => date}).
    {ok, <<"1976-01-08">>}

    > matcher:validate(<<"1976-01-08T00:59:32Z">>, #{type => utf8_binary, format => datetime}).
    {ok, <<"1976-01-08T00:59:32Z">>}

    > matcher:validate(<<"1976-01-08T22:59:59.123456">>, #{type => utf8_binary, format => datetime}).
    {ok, <<"1976-01-08T22:59:59.123456">>}

    > matcher:validate(<<"john.doe@localhost.localdomain">>, #{type => utf8_binary, nullable => false, format => email}).
    {ok, <<"john.doe@localhost.localdomain">>}

    > matcher:validate(<<"uk">>, #{type => utf8_binary, format => lang_alpha2}).
    {ok, <<"uk">>},

    > matcher:validate(<<"eng">>, #{type => utf8_binary, format => lang_alpha3}).
    {ok, <<"eng">>}

    > matcher:validate(<<"UA">>, #{type => utf8_binary, format => country_alpha2}).
    {ok, <<"UA">>}

    > matcher:validate(<<"GBR">>, #{type => utf8_binary, format => country_alpha3}).
    {ok, <<"GBR">>}
```

#### Negative cases
```
    > matcher:validate(null, #{type => utf8_binary}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(null, #{type => utf8_binary, nullable => false}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(some_atom2, #{type => utf8_binary}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(1, #{type => utf8_binary}).
    {error, {not_valid, [], wrong_type}}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, min_length => 7}).
    {error, {not_valid, [], too_short}}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, max_length => 5}).
    {error, {not_valid, [], too_long}}

    > matcher:validate(<<>>, #{type => utf8_binary, pattern => <<"^[0-9]+">>}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(<<"Привет"/utf8>>, #{type => utf8_binary, pattern => <<"^[0-9]+">>}).
    {error, {not_valid, [], not_match}}

    % Just wrong format
    > matcher:validate(<<"1976:01:08">>, #{type => utf8_binary, format => date}).
    {error, {not_valid, [], incorrect_format}}

    % A datetime is not a date
    > matcher:validate(<<"1976-01-08T02:13:10">>, #{type => utf8_binary, format => date}).
    {error, {not_valid, [], incorrect_format}}

    % It is not a leap year
    > matcher:validate(<<"1981-02-29">>, #{type => utf8_binary, format => date}).
    {error, {not_valid, [], incorrect_format}}

    % The value must be a binary string
    > matcher:validate("1976-01-08", #{type => utf8_binary, format => date}).
    {error, {not_valid, [], wrong_type}}

    % Incorrect month
    > matcher:validate(<<"1981-13-20T10:01:34">>, #{type => utf8_binary, format => datetime}).
    {error, {not_valid, [], incorrect_format}}

    % At least two subdomains expected
    > matcher:validate(<<"john.doe@localhost">>, #{type => utf8_binary, nullable => false, format => email}).
    {error, {not_valid, [], incorrect_format}}

    % Very long domin
    > matcher:validate(<<"john.doe@veeeeeeeeeeeeeeeeeeeeeeeeeeeerrylooooooooooooooooooooooooooooong.domain">>,
                       #{type => utf8_binary, format => email}).
    {error, {not_valid, [], incorrect_format}}

    % Subdomain must not be started with "-"
    > matcher:validate(<<"john.doe@-24h.org">>, #{type => utf8_binary, format => email}).
    {error, {not_valid, [], incorrect_format}}

    % Root domain must be two or more symbols
    > matcher:validate(<<"john.doe@localhost.l">>, #{type => utf8_binary, format => email}).
    {error, {not_valid, [], incorrect_format}}

    % Exactly two symbols expected
    > matcher:validate(<<"u">>, #{type => utf8_binary, format => lang_alpha2}).
    {error, {not_valid, [], incorrect_format}}

    % Must be lowercase
    > matcher:validate(<<"UK">>, #{type => utf8_binary, format => lang_alpha2}).
    {error, {not_valid, [], incorrect_format}}

    % Only latin characters allowed
    > matcher:validate(<<"uk1">>, #{type => utf8_binary, format => lang_alpha3}).
    {error, {not_valid, [], incorrect_format}}

    % No spaces allowed
    > matcher:validate(<<"U ">>, #{type => utf8_binary, format => country_alpha2}).
    {error, {not_valid, [], incorrect_format}}

    % Must be uppercase
    > matcher:validate(<<"Uan">>, #{type => utf8_binary, format => country_alpha3}).
    {error, {not_valid, [], incorrect_format}}
```

### Integer
#### Positive cases
```
    > matcher:validate(0, #{type => integer}).
    {ok, 0}

    > matcher:validate(-12345, #{type => integer}).
    {ok, -12345}

    > matcher:validate(3, #{type => integer, minimum => 2, maximum => 4}).
    {ok, 3}

    > matcher:validate(-8, #{type => integer, divisible_by => 4}).
    {ok, -8}

    > matcher:validate(25, #{type => integer, divisible_by => 5, minimum => 24, maximum => 29}).
    {ok, 25}

    > matcher:validate(127, #{type => integer, format => int8}).
    {ok, 127}

    > matcher:validate(-32768, #{type => integer, format => int16}).
    {ok, -32768}

    > matcher:validate(2147483647, #{type => integer, format => int32}).
    {ok, 2147483647}

    > matcher:validate(-9223372036854775808, #{type => integer, format => int64}).
    {ok, -9223372036854775808}

    > matcher:validate(255, #{type => integer, format => uint8}).
    {ok, 255}

    > matcher:validate(65535, #{type => integer, format => uint16}).
    {ok, 65535}

    > matcher:validate(4294967295, #{type => integer, format => uint32}).
    {ok, 4294967295}

    > matcher:validate(18446744073709551615, #{type => integer, format => uint64}).
    {ok, 18446744073709551615}
```

#### Negative ceses
```
    % A float is not an integer
    > matcher:validate(12345.0, #{type => integer}).
    {error, {not_valid, [], wrong_type}}

    % Mast be integer type
    > matcher:validate("12345", #{type => integer}).
    {error, {not_valid, [], wrong_type}}

    % Less than minimum
    > matcher:validate(-1, #{type => integer, minimum => 0}).
    {error, {not_valid, [], too_small}}

    % Bigger than maximim
    > matcher:validate(11, #{type => integer, maximum => 10}).
    {error, {not_valid, [], too_big}}

    % Out of range
    > matcher:validate(1, #{type => integer, minimum => 2, maximum => 4}).
    {error, {not_valid, [], too_small}}

    % Not divisible by the number
    > matcher:validate(-7, #{type => integer, divisible_by => 4}).
    {error, {not_valid, [], not_divisible}}

    % Out of range
    > matcher:validate(15, #{type => integer, divisible_by => 5, minimum => 24, maximum => 29}).
    {error, {not_valid, [], too_small}}

    % Out of range
    > matcher:validate(-129, #{type => integer, format => int8}).
    {error, {not_valid, [], incorrect_format}}

    % Out of range
    > matcher:validate(-1, #{type => integer, format => uint64}).
    {error, {not_valid, [], incorrect_format}}

    % Out of range
    > matcher:validate(18446744073709551616, #{type => integer, format => uint64}).
    {error, {not_valid, [], incorrect_format}}
```

### Number
#### Positive cases
```
    > matcher:validate(12345, #{type => number}).
    {ok, 12345}

    > matcher:validate(12345.678, #{type => number}).
    {ok, 12345.678}

    > matcher:validate(99.9999999999, #{type => number, maximum => 100, exclusive_maximum => true}).
    {ok, 99.9999999999}

    > matcher:validate(-2.0, #{type => number, minimum => -2.0, maximum => 4.0}).
    {ok, -2.0}
```

#### Negative cases
```
    % Mast be integer type
    > matcher:validate("12345", #{type => number}).
    {error, {not_valid, [], wrong_type}}

    % Less than minimum
    > matcher:validate(-7, #{type => number, minimum => -6}).
    {error, {not_valid, [], too_small}}

    % Bigger than maximim
    > matcher:validate(-10, #{type => number, maximum => -10, exclusive_maximum => true}).
    {error, {not_valid, [], too_big}}

    % Out of range
    > matcher:validate(1.99999999999999, #{type => number, minimum => 2, maximum => 4}).
    {error, {not_valid, [], too_small}}
```

### Decimal binary
#### Positive cases
```
    > matcher:validate(<<"6.22">>, #{type => decimal_binary, minimum => <<"-5.33">>}).
    {ok, <<"6.22">>}

    > matcher:validate(<<"3.0">>, #{type => decimal_binary, minimum => <<"2.1">>, maximum => <<"4.567">>}).
    {ok, <<"3.0">>}

    > matcher:validate(<<"12.245">>, #{type => decimal_binary, precision => 4}).
    {ok, <<"12.245">>}

    > matcher:validate(<<"1200">>, #{type => decimal_binary, precision => -2}).
    {ok, <<"1200">>}
```

#### Negative cases
```
    % Less than minimum
    > matcher:validate(<<"-6.31">>, #{type => decimal_binary, minimum => <<"-6.3">>}).
    {error, {not_valid, [], too_small}}

    % Bigger than maximim
    > matcher:validate(<<"10.1234">>, #{type => decimal_binary, maximum => <<"10.123">>}).
    {error, {not_valid, [], too_big}}

    % Out of range
    > matcher:validate(<<"4.561">>, #{type => decimal_binary, minimum => <<"2.23">>, maximum => <<"4.56">>}).
    {error, {not_valid, [], too_big}}

    % Too much precision
    > matcher:validate(<<"3.45678">>, #{type => decimal_binary, precision => 4}).
    {error, {not_valid, [], incorrect_precision}}
```

### Boolean
#### Positive cases
```
    > matcher:validate(null, #{type => boolean, nullable => true}).
    {ok, null}

    > matcher:validate(true, #{type => boolean}).
    {ok, true}

    > matcher:validate(false, #{type => boolean}).
    {ok, false}
```

#### Negative cases
```
    > matcher:validate(12345.0, #{type => boolean}).

    {error, {not_valid, [], wrong_type}}
    > matcher:validate(<<"2020:12:31">>, #{type => boolean})),

    {error, {not_valid, [], wrong_type}}
    > matcher:validate(1999, #{type => boolean})),

    {error, {not_valid, [], wrong_type}}

    > matcher:validate(flase, #{type => boolean})),
    {error, {not_valid, [], wrong_type}}
```

### Term
#### Positive cases
 ```
   > matcher:validate(<<"12345">>, #{type => term}).
    {ok, <<"12345">>}

    > matcher:validate(some_atom, #{type => term}).
    {ok, some_atom}

    > matcher:validate(#{key => "value"}, #{type => term}).
    {ok, #{key := "value"}}

    > matcher:validate(<<"2020-12-31">>, #{type => term, validator => fun matcher_lib:is_date/1}).
    {ok, <<"2020-12-31">>}
```

#### Negative cases
```
    > matcher:validate(12345.0, #{type => term, validator => fun erlang:is_integer/1}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(<<"2020:12:31">>, #{type => term, validator => fun matcher_lib:is_date/1}).
    {error, {not_valid, [], not_match}}

    > matcher:validate(1999, #{type => term, validator => fun matcher_lib:is_date/1}).
    {error, {not_valid, [], not_match}}
```

### Tuple
#### Positive cases
```
    > matcher:validate({}, #{type => tuple}).
    {ok, {}}

    > matcher:validate({1}, #{type => tuple}).
    {ok, {1}}

    > matcher:validate({1, a}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]}).
    {ok, {1, a}}
```

#### Negative cases
```
    % The value must be a tuple
    > matcher:validate(12345.0, #{type => tuple, nullable => false}).
    {error, {not_valid, [], wrong_type}}

    % The tuple size is not equal the schema size
    > matcher:validate({1}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]}).
    {error, {not_valid, [], wrong_size}}

    % The second element is invalid
    > matcher:validate({1, b}, #{type => tuple, schema => [#{type => integer}, #{type => const, value => a}]}).
    {error, {not_valid, [{te, 2}], not_match}}
```

### List
#### Positive cases
```
    > matcher:validate([], #{type => list}).
    {ok, []}

    > matcher:validate([1], #{type => list}).
    {ok, [1]}

    > matcher:validate([a, b, 1, 2, #{}], #{type => list}).
    {ok, [a, b, 1, 2, #{}]}

    > matcher:validate([1, a], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]}).
    {ok, [1, a]}

    > matcher:validate([1, a, a], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]}).
    {ok, [1, a, a]}

    > matcher:validate([a], #{type => list, max_length => 1}).
    {ok, [a]}

    > matcher:validate([a, "b", <<"c">>], #{type => list, min_length => 2, max_length => 3, schema => [#{type => term}]}).
    {ok, [a, "b", <<"c">>]}
```

#### Negative cases
```
    % The value must be a list
    > matcher:validate(12345.0, #{type => list, nullable => false}).
    {error, {not_valid, [], wrong_type}}

    % Wrong length
    > matcher:validate([1, 2], #{type => list, min_length => 3, schema => [#{type => integer}]}).
    {error, {not_valid, [], too_short}}

    % The list item is invalid
    > matcher:validate([1], #{type => list, schema => [#{type => const, value => a}]}).
    {error, {not_valid, [{li, 1}], not_match}}
 
    % The second item is invalid
    > matcher:validate([1, b], #{type => list, schema => [#{type => integer}, #{type => const, value => a}]}).
    {error, {not_valid, [{li, 2}], not_match}}
```

### Map
#### Positive cases
```
    % Check validation without a schema
    ?assertMatch({ok, #{}},
                 matcher:validate(#{}, #{type => map})),

    % Check default option
    > matcher:validate(#{key1 => "some_value"},
                       #{type => map, schema =>
                            #{key1 => #{descriptor => #{type => list}},
                              <<"key2">> => #{optional => true, default => 4, descriptor => #{type => term}}
                             }}).
    {ok, #{key1 := "some_value", <<"key2">> := 4}}

    % A schema with descriptors
    > matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                       #{type => map, schema =>
                             #{key1 => #{descriptor => #{type => list}},
                               <<"key2">> => #{descriptor => #{type => boolean}}}}).
    {ok, #{key1 := "some_value", <<"key2">> := true}}

    % The requires and conflicts options
    > matcher:validate(#{key1 => "some_value", <<"key2">> => true},
                       #{type => map, schema =>
                             #{key1 => #{optional => true, requires => [<<"key2">>], descriptor => #{type => list}},
                               <<"key2">> => #{optional => true, conflicts => ["key3"], descriptor => #{type => boolean}},
                               "key3" => #{optional => true}}}).
    {ok, #{key1 := "some_value", <<"key2">> := true}}

    % Validation of nested maps
    >  matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => #{k1 => 1, k2 => 2}},
                        #{type => map, schema =>
                              #{key1 => #{requires => [<<"key2">>], descriptor => #{type => list}},
                                <<"key2">> => #{descriptor => #{type => boolean}},
                                "key3" => #{descriptor => #{type => map, schema =>
                                                                #{k1 => #{descriptor => #{type => const, value => 1}},
                                                                  k2 => #{}}}}}}).
    {ok, #{key1 := "some_value", <<"key2">> := true, "key3" := #{k1 := 1, k2 := 2}}}

    % allow_extra_keys option with extra keys
    > matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                       #{type => map, allow_extra_keys => true, schema => #{key1 => #{}, <<"key2">> => #{}}}).
    {ok, #{key1 := "some_value", <<"key2">> := true, "key3" := 1}}

    % extra_keys_descriptor option with extra keys
    > matcher:validate(#{key1 => "some_value", "key2" => true, "key3" => 1},
                       #{type => map, extra_keys_descriptor => #{type => list}, schema => #{key1 => #{}}}).
    #{key1 := "some_value", "key2" := true, "key3" := 1}}

    % extra_values_descriptor option with extra keys
    > matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                       #{type => map, extra_values_descriptor => #{type => integer}, schema => #{key1 => #{}}}).
    {ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}}

    % Check min_extra_keys option
    > matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                       #{type => map, min_extra_keys => 3}).
    {ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}}

    % Check max_extra_keys option
    > matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 10},
                       #{type => map, max_extra_keys => 3}).
    {ok, #{key1 := "some_value", <<"key2">> := 4, "key3" := 10}}
```

#### Negative cases
```
    % There is no key1 in the map
    > matcher:validate(#{}, #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}}}).
    {error, {not_valid, [{mk, key1}], key_missed}}

    % There is extra key key2 in the map
    > matcher:validate(#{key1 => 1, key2 => 2}, #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}}}).
    {error, {not_valid, [{mk, key2}], extra_keys_not_allowed}}

    % Incorrect key value type
    > matcher:validate(#{key1 => true, key2 => 2},
                       #{type => map, schema => #{key1 => #{descriptor => #{type => integer}}, key2 => #{}}}).
    {error, {not_valid, [{mv, key1}], wrong_type}}

    % There is no a required key
    > matcher:validate(#{key1 => 1},
                       #{type => map, schema => #{key1 => #{requires => [key2], descriptor => #{type => integer}},
                                                  key2 => #{optional => true}}}).
    {error, {not_valid, [{mk, key1}], missing_required}}
 
    % There is a confict key
    > matcher:validate(#{key1 => 1, key2 => 1},
                       #{type => map, schema => #{key1 => #{conflicts => [key2], descriptor => #{type => integer}},
                                                  key2 => #{optional => true}}}).
    {error, {not_valid, [{mk, key1}], conflicts}}
 
    % extra_keys_descriptor
    > matcher:validate(#{key1 => "some_value", <<"key2">> => true, "key3" => 1},
                       #{type => map, extra_keys_descriptor => #{type => list}, schema => #{key1 => #{}}}).
    {error, {not_valid, [{mk, <<"key2">>}], wrong_type}}

    % max_extra_keys option 
    > matcher:validate(#{key1 => "some_value", <<"key2">> => 4, "key3" => 1},
                       #{type => map, max_extra_keys => 2}).
    {error, {not_valid, [], too_much_extra_keys}}
```

### Alternative types
#### Positive cases
```
    % Simple schema with two alternatives
    > matcher:validate(1, #{type => alt, schema => [{#{type => integer}, #{type => integer}},
                                                    {#{type => const, value => a}, #{type => term}}]}).
    {ok, 1}
    > matcher:validate(a, #{type => alt, schema => [{#{type => integer}, #{type => integer}},
                                                    {#{type => const, value => a}, #{type => term}}]}).
    {ok, a}
```

#### Negative cases
```
    % Both pre-descriptors failed
    > matcher:validate(1.0, #{type => alt, schema => [{#{type => integer}, #{type => integer, minimum => 10}},
                                                      {#{type => const, value => a}, #{type => integer}}]}).
    {error, {not_valid, [], alternatives_not_match}}

    % The first pre-descriptor successed but the essential descriptor failed
    > matcher:validate(5, #{type => alt, schema => [{#{type => integer}, #{type => integer, minimum => 10}},
                                                    {#{type => const, value => a}, #{type => integer}}]}).
    {error, {not_valid, [], too_small}}
```

### Alternatives types (simple)
#### Positive cases
```
    % Simple schema with two alternatives
    > matcher:validate(1, #{type => alt_simple, schema => [#{type => integer},
                                                           #{type => const, value => a}]}).
    {ok, 1}

    > matcher:validate(a, #{type => alt_simple, schema => [#{type => integer},
                                                           #{type => const, value => a}]}).
    {ok, a}
```

#### Negative ceses
 ```
   % Both descriptors failed
    > matcher:validate(1.0, #{type => alt_simple, schema => [#{type => integer},
                                                             #{type => const, value => a}]}).
    {error, {not_valid, [], alternatives_not_match}}

    % Both descriptors failed
    > matcher:validate(5, #{type => alt_simple, schema => [#{type => integer, minimum => 10},
                                                           #{type => const, value => a}]}).
    {error, {not_valid, [], alternatives_not_match}}
```

### Converters
#### Positive cases
```
    % Using a pre_converter
    > matcher:validate(1, #{type => utf8_binary, pre_converter => fun integer_to_binary/1}).
    {ok, <<"1">>}

    % Using post_converter
    > matcher:validate(1, #{type => integer, post_converter => fun integer_to_binary/1}).
    {ok, <<"1">>}

    % Both converters work togather
    > matcher:validate(1, #{type => utf8_binary, pre_converter => fun integer_to_binary/1, post_converter => fun binary_to_list/1}).
    {ok, "1"}
```

#### Negative cases
```
    % The pre-converter converts to a wrong type
    > matcher:validate(1, #{type => integer, pre_converter => fun list_to_float/1}).
    {error, {not_valid, [], not_match}}

    % The post-converter failed to convert
    > matcher:validate(true, #{type => boolean, post_converter => fun list_to_binary/1})),
    {error, {not_valid, [], not_match}}
```

Types
-----
```
-type descriptor() ::alt_descriptor() |  alt_simple_descriptor() | map_descriptor() | list_descriptor() |
          tuple_descriptor() | number_descriptor() | integer_descriptor() | utf8_binary_descriptor() |
          const_descriptor() | enum_descriptor() | binary_descriptor() | decimal_binary_descriptor() |
          boolean_descriptor() | term_descriptor() | custom_descriptor().
```
```
-type alt_descriptor() ::
          #{
            type := alt,
            schema := list({Match::descriptor(), Essential::descriptor()}),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type alt_simple_descriptor() ::
          #{
            type := alt_simple,
            schema := list(descriptor()),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type map_descriptor() ::
          #{
            type := map,
            schema => map_schema(),
            extra_keys_descriptor => descriptor(),
            extra_values_descriptor => descriptor(),
            max_extra_keys => non_neg_integer(),
            min_extra_keys => non_neg_integer(),
            allow_extra_keys => boolean(), % default - false
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type list_descriptor() ::
          #{
            type := list,
            schema => list(descriptor()),
            min_length => non_neg_integer(),
            max_length => non_neg_integer(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type tuple_descriptor() ::
          #{
            type := tuple,
            schema => list(descriptor()),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type number_descriptor() ::
          #{
            type := number,
            minimum => number(),
            exclusive_mininum => boolean(), % default - false
            maximum => number(),
            exclusive_maximum => boolean(), % default - false
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type integer_descriptor() ::
          #{
            type := integer,
            format => int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64,
            minimum => integer(),
            maximum => integer(),
            divisible_by => pos_integer(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type utf8_binary_descriptor() ::
          #{
            type := utf8_binary,
            format => date | datetime | email | lang_alpha2 | lang_alpha3 | country_alpha2 | country_alpha3,
            min_length => non_neg_integer(),
            max_length => non_neg_integer(),
            pattern => binary(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type const_descriptor() ::
          #{
            type := const,
            value => term(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type enum_descriptor() ::
          #{
            type := enum,
            values => list(term()),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type binary_descriptor() ::
          #{
            type := binary,
            min_size => non_neg_integer(),
            max_size => non_neg_integer(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type decimal_binary_descriptor() ::
          #{
            type := decimal_binary,
            minimum => binary(),
            maximum => binary(),
            precision => integer(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type boolean_descriptor() ::
          #{
            type := boolean,
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type term_descriptor() ::
          #{
            type := term,
            validator := validator_fun(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.

-type custom_descriptor() ::
          #{
            type := custom,
            name := binary(),
            custom_types => custom_types(),
            pre_converter => converter_fun(),
            post_converter => converter_fun(),
            nullable => boolean(), % default - false
            meta => term()
           }.
```
```
-type validator_fun() :: fun((any()) -> boolean() | no_return()).

-type converter_fun() :: fun((any()) -> any() | no_return()).

-type map_schema() :: #{Key :: term() => map_key_descriptor()}.

-type map_key_descriptor() ::
          #{
            descriptor => descriptor(),
            optional => boolean(),
            default => any(),
            conflicts => list(),
            requires => list()
           }.

-type custom_types() :: #{Type::binary() => descriptor()}.
-type options() :: #{extra_custom_types => custom_types()}.
```
```
-type path() :: list({mk, term()} | {mv, term()} | {li, pos_integer()} | {te, pos_integer()}).
```
```
-type error() :: not_valid_error() | bad_descriptor_error().

-type not_valid_error() :: {not_valid, path(), not_valid_reason()}.

-type not_valid_reason() :: wrong_type | too_small | too_big | not_utf8 |
          too_short | too_long | not_match | incorrect_format | not_divisible | not_decimal |
          incorrect_precision | wrong_size | extra_keys_not_allowed | not_enough_extra_keys |
          too_much_extra_keys | key_missed | conflicts | missing_required.

-type bad_descriptor_error() :: {bad_descriptor, path(), bad_descriptor_reason()}.

-type bad_descriptor_reason() :: no_type | invalid_min_size | invalid_max_size | invalid_min_length | invalid_max_length |
          invalid_pattern | unknown_format | invalid_minimum | invalid_maximum | invalid_divisible_by |
          invalid_exclusive_minimum | invalid_exclusive_maximum | invalid_precision | invalid_validator |
          invalid_schema | invalid_min_extra_keys | invalid_max_extra_keys | invalid_extra_keys_descriptor |
          invalid_extra_values_descriptor | unsupported_type | invalid_nullable | value_missed | values_missed |
          invalid_value | invalid_values | invalid_conflicts | invalid_requires | invalid_descriptor |
          invalid_allow_extra_keys | no_schema | no_name | invalid_custom_types | invalid_pre_converter |
          invalid_post_converter.
```
```
-type matchers() :: #{Type :: atom() => MatcherFun :: function()}. % TODO: Full spec needed
```

