%% @author serge
%% @doc @todo Add description to matcher_descriptor.


-module(matcher_descriptor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([validate_descriptor/1]).

-export([descriptor_descriptor/0]).


-spec validate_descriptor(matcher:descriptor()) -> ok | {error, matcher:error()}.
validate_descriptor(DescriptorValue) ->
    case matcher:validate(DescriptorValue, descriptor_descriptor()) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec descriptor_descriptor() -> matcher:descriptor().
descriptor_descriptor() ->
    #{type => custom,
      name => <<"descriptor">>,
      custom_types =>
          #{<<"descriptor">> =>
                #{type => alt, schema =>
                      [
                       {pre_descriptor(alt), alt_descriptor()},
                       {pre_descriptor(alt_simple), alt_simple_descriptor()},
                       {pre_descriptor(map), map_descriptor()},
                       {pre_descriptor(list), list_descriptor()},
                       {pre_descriptor(tuple), tuple_descriptor()},
                       {pre_descriptor(number), number_descriptor()},
                       {pre_descriptor(integer), integer_descriptor()},
                       {pre_descriptor(utf8_binary), utf8_binary_descriptor()},
                       {pre_descriptor(const), const_descriptor()},
                       {pre_descriptor(enum), enum_descriptor()},
                       {pre_descriptor(binary), binary_descriptor()},
                       {pre_descriptor(decimal_binary), decimal_binary_descriptor()},
                       {pre_descriptor(boolean), boolean_descriptor()},
                       {pre_descriptor(term), term_descriptor()},
                       {pre_descriptor(custom), custom_descriptor()}
                      ]
                 }
           }
     }.

-spec pre_descriptor(Type :: atom()) -> matcher:descriptor().
pre_descriptor(Type) ->
    #{type => map, allow_extra_keys => true, schema => #{type => #{descriptor => #{type => const, value => Type}}}}.


-spec alt_descriptor() -> matcher:descriptor().
alt_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => alt}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            schema => #{descriptor => #{type => list, schema =>
                                            [#{type => tuple, schema =>
                                                   [#{type => custom, name => <<"descriptor">>},
                                                    #{type => custom, name => <<"descriptor">>}]}]}}
           }
     }.


-spec alt_simple_descriptor() -> matcher:descriptor().
alt_simple_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => alt_simple}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            schema => #{descriptor => #{type => list, schema => [#{type => custom, name => <<"descriptor">>}]}}
           }}.


-spec map_descriptor() -> matcher:descriptor().
map_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => map}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            schema => #{optional => true, descriptor =>
                            #{type => map, extra_values_descriptor =>
                                  #{type => map, schema =>
                                        #{
                                          descriptor => #{optional => true,
                                                          descriptor => #{type => custom, name => <<"descriptor">>}},
                                          optional => #{optional => true, descriptor => #{type => boolean}},
                                          default => #{optional => true, descriptor => #{type => term}},
                                          conflicts => #{optional => true, descriptor => #{type => list}},
                                          requires => #{optional => true, descriptor => #{type => list}}
                                         }
                                   }}},
            extra_keys_descriptor => #{optional => true, descriptor => #{type => custom, name => <<"descriptor">>}},
            extra_values_descriptor => #{optional => true, descriptor => #{type => custom, name => <<"descriptor">>}},
            max_extra_keys => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            min_extra_keys => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            allow_extra_keys => #{optional => true, descriptor => #{type => boolean}}
           }}.


-spec list_descriptor() -> matcher:descriptor().
list_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => list}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            schema => #{optional => true, descriptor =>
                            #{type => list, schema => [#{type => custom, name => <<"descriptor">>}]}},
            min_length => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            max_length => #{optional => true, descriptor => #{type => integer, minimum => 0}}
           }}.


-spec tuple_descriptor() -> matcher:descriptor().
tuple_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => tuple}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            schema => #{optional => true, descriptor => #{type => list, schema =>
                                                              [#{type => custom, name => <<"descriptor">>}]}}
           }}.


-spec number_descriptor() -> matcher:descriptor().
number_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => number}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            minimum => #{optional => true, descriptor => #{type => number}},
            exclusive_mininum => #{optional => true, descriptor => #{type => boolean}},
            maximum => #{optional => true, descriptor => #{type => number}},
            exclusive_maximum => #{optional => true, descriptor => #{type => boolean}}
           }}.


-spec integer_descriptor() -> matcher:descriptor().
integer_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => integer}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            format => #{optional => true, descriptor => #{type => enum, values => [int8, int16, int32, int64,
                                                                                   uint8, uint16, uint32, uint64]}},
            minimum => #{optional => true, descriptor => #{type => integer}},
            maximum => #{optional => true, descriptor => #{type => integer}},
            divisible_by => #{optional => true, descriptor => #{type => integer, minimum => 1}}
           }}.


-spec utf8_binary_descriptor() -> matcher:descriptor().
utf8_binary_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => utf8_binary}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            format => #{optional => true,
                        descriptor => #{type => enum, values => [date, datetime, email, lang_alpha2, lang_alpha3,
                                                                 country_alpha2, country_alpha3]}},
            min_length => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            max_length => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            pattern => #{optional => true, descriptor => #{type => utf8_binary}}
           }}.


-spec const_descriptor() -> matcher:descriptor().
const_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => const}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            value => #{descriptor => #{type => term}}
           }}.

-spec enum_descriptor() -> matcher:descriptor().
enum_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => enum}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            values => #{descriptor => #{type => list, min_length => 1}}
           }}.


-spec binary_descriptor() -> matcher:descriptor().
binary_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => binary}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            min_size => #{optional => true, descriptor => #{type => integer, minimum => 0}},
            max_size => #{optional => true, descriptor => #{type => integer, minimum => 0}}
           }}.


-spec decimal_binary_descriptor() -> matcher:descriptor().
decimal_binary_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => decimal_binary}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            minimum => #{optional => true, descriptor => #{type => decimal_binary}},
            maximum => #{optional => true, descriptor => #{type => decimal_binary}},
            precision => #{optional => true, descriptor => #{type => integer}}
           }}.


-spec boolean_descriptor() -> matcher:descriptor().
boolean_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => boolean}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}}
           }}.


-spec term_descriptor() -> matcher:descriptor().
term_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => term}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            validator =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}}
           }}.


-spec custom_descriptor() -> matcher:descriptor().
custom_descriptor() ->
    #{type => map, schema =>
          #{
            type => #{descriptor => #{type => const, value => custom}},
            pre_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            post_converter =>
                #{optional => true, descriptor => #{type => term, validator => fun(X) -> is_function(X, 1) end}},
            nullable => #{optional => true, descriptor => #{type => boolean}},
            meta => #{optional => true, descriptor => #{type => term}},
            name => #{descriptor => #{type => utf8_binary}},
            custom_types => #{optional => true,
                              descriptor => #{type => map,
                                              extra_keys_descriptor => #{type => utf8_binary},
                                              extra_values_descriptor => #{type => custom, name => <<"descriptor">>}
                                             }}
           }}.


