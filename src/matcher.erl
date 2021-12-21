%% @author serge
%% @doc @todo Add description to matcher.


-module(matcher).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         validate/2,
         validate/3,
         validate/4
        ]).

-export_type([
              descriptor/0,
              custom_types/0,
              path/0,
              error/0
             ]).

-type descriptor() ::alt_descriptor() |  alt_simple_descriptor() | map_descriptor() | list_descriptor() |
          tuple_descriptor() | number_descriptor() | integer_descriptor() | utf8_binary_descriptor() |
          const_descriptor() | enum_descriptor() | binary_descriptor() | decimal_binary_descriptor() |
          boolean_descriptor() | term_descriptor() | custom_descriptor().

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


-type path() :: list({mk, term()} | {mv, term()} | {li, pos_integer()} | {te, pos_integer()}).

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

-type matchers() :: #{Type :: atom() => MatcherFun :: function()}. % TODO: Full spec needed

-spec validate(term(), descriptor()) -> {ok, term()} | {error, error()}.
validate(Value, Descriptor) ->
    validate(Value, Descriptor, #{}).

-spec validate(term(), descriptor(), options()) -> {ok, term()} | {error, error()}.
validate(Value, Descriptor, Options) ->
    validate(Value, Descriptor, Options, #{}).

-spec validate(term(), descriptor(), options(), matchers()) -> {ok, term()} | {error, error()}.
validate(Value, Descriptor, Options, ExtraMatchers) ->
    is_map(Options) orelse error(badarg),
    try
        Matchers = maps:merge(ExtraMatchers, default_matchers()),
        {ok, validate_item(Descriptor, Value, Options, _Path = [], Matchers)}
    catch
        throw:Error ->
            {error, Error}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

default_matchers() ->
    #{alt => fun validate_alt/5,
      alt_simple => fun validate_alt_simple/5,
      map => fun validate_map/5,
      list => fun validate_list/5,
      tuple => fun validate_tuple/5,
      number => fun validate_number/5,
      integer => fun validate_integer/5,
      utf8_binary => fun validate_utf8_binary/5,
      const => fun validate_const/5,
      enum => fun validate_enum/5,
      binary => fun validate_binary/5,
      decimal_binary => fun validate_decimal_binary/5,
      boolean => fun validate_boolean/5,
      term => fun validate_term/5,
      custom => fun validate_custom/5
     }.


-spec validate_item(descriptor(), term(), options(), path(), matchers()) -> term() | no_return().
validate_item(Descriptor, Value, Options, Path, Matchers) ->
    case Descriptor of
        #{type := Type} ->
            case maps:find(Type, Matchers) of
                {ok, MatcherFun} ->
                    case maps:get(nullable, Descriptor, false) of
                        true when Value == null ->
                            Value;
                        Nullable when is_boolean(Nullable) ->
                            ValueIn = validate_by_option2(pre_converter, Descriptor, Path,
                                                          fun(ConverterFun) when is_function(ConverterFun, 1) -> ConverterFun end,
                                                          invalid_pre_converter,
                                                          fun(ConverterFun) -> ConverterFun(Value) end,
                                                          not_match,
                                                          Value),
                            ValueOut = MatcherFun(Descriptor, ValueIn, Options, Path, Matchers),
                            validate_by_option2(post_converter, Descriptor, Path,
                                                fun(ConverterFun) when is_function(ConverterFun, 1) -> ConverterFun end,
                                                invalid_post_converter,
                                                fun(ConverterFun) -> ConverterFun(ValueOut) end,
                                                not_match,
                                                ValueOut);
                        _ ->
                            throw_error(bad_descriptor, Path, invalid_nullable)
                    end;
                error ->
                    throw_error(bad_descriptor, Path, unsupported_type)
            end;
        #{} ->
            throw_error(bad_descriptor, Path, no_type);
        _ ->
            throw_error(bad_descriptor, Path, invalid_descriptor)
    end.

-spec validate_const(descriptor(), term(), options(), path(), matchers()) -> term() | no_return().
validate_const(#{type := const} = Descriptor, Value, _Options, Path, _Matchers) ->
    maps:is_key(value, Descriptor) orelse throw_error(bad_descriptor, Path, value_missed),
    validate_by_option(value, Descriptor, Path,
                       fun(ConstValue) -> ConstValue end,
                       invalid_value, % In this case the error will not be used
                       fun(ConstValue) when Value =:= ConstValue -> ok end,
                       not_match),
    Value.

validate_enum(#{type := enum} = Descriptor, Value, _Options, Path, _Matchers) ->
    maps:is_key(values, Descriptor) orelse throw_error(bad_descriptor, Path, values_missed),
    validate_by_option(values, Descriptor, Path,
                       fun(Values) when is_list(Values), length(Values) > 0 -> Values end,
                       invalid_values,
                       fun(Values) -> true = lists:member(Value, Values) end,
                       not_match),
    Value.

validate_binary(#{type := binary} = Descriptor, Value, _Options, Path, _Matchers) ->
    is_binary(Value) orelse throw_error(not_valid, Path, wrong_type),
    Size = erlang:byte_size(Value),
    validate_by_option(min_size, Descriptor, Path,
                       fun(MinSize) when is_integer(MinSize), MinSize >= 0 -> MinSize end,
                       invalid_min_size,
                       fun(MinSize) when Size >= MinSize -> ok end,
                       too_small),
    validate_by_option(max_size, Descriptor, Path,
                       fun(MaxSize) when is_integer(MaxSize), MaxSize >= 0 -> MaxSize end,
                       invalid_max_size,
                       fun(MaxSize) when Size =< MaxSize -> ok end,
                       too_big),
    Value.

validate_utf8_binary(#{type := utf8_binary} = Descriptor, Value, _Options, Path, _Matchers) ->
    is_binary(Value) orelse throw_error(not_valid, Path, wrong_type),
    Length = try length(string:to_graphemes(Value)) catch _:_ -> throw_error(not_valid, Path, not_utf8) end,
    validate_by_option(min_length, Descriptor, Path,
                       fun(MinLength) when is_integer(MinLength), MinLength >= 0 -> MinLength end,
                       invalid_min_length,
                       fun(MinLength) when Length >= MinLength -> ok end,
                       too_short),
    validate_by_option(max_length, Descriptor, Path,
                       fun(MaxLength) when is_integer(MaxLength), MaxLength >= 0 -> MaxLength end,
                       invalid_max_length,
                       fun(MaxLength) when Length =< MaxLength -> ok end,
                       too_long),
    validate_by_option(pattern, Descriptor, Path,
                       fun(Pattern) when is_binary(Pattern) ->
                               {ok, MP} = re:compile(Pattern),
                               MP
                       end,
                       invalid_pattern,
                       fun(MP) -> match = re:run(Value, MP, [global, {capture, none}]) end,
                       not_match),
    validate_by_option(format, Descriptor, Path,
                       fun(date) -> fun matcher_lib:is_date/1;
                          (datetime) -> fun matcher_lib:is_iso8601_datetime/1;
                          (email) -> fun matcher_lib:is_email/1;
                          (lang_alpha2) -> fun matcher_lib:is_language_alpha2/1;
                          (lang_alpha3) -> fun matcher_lib:is_language_alpha3/1;
                          (country_alpha2) -> fun matcher_lib:is_country_alpha2/1;
                          (country_alpha3) -> fun matcher_lib:is_country_alpha3/1
                       end,
                       unknown_format,
                       fun(ValidateFun) -> true = ValidateFun(Value) end,
                       incorrect_format),
    Value.

validate_integer(#{type := integer} = Descriptor, Value, _Options, Path, _Matchers) ->
    is_integer(Value) orelse throw_error(not_valid, Path, wrong_type),
    validate_by_option(minimum, Descriptor, Path,
                       fun(Min) when is_integer(Min) -> Min end,
                       invalid_minimum,
                       fun(Min) when Value >= Min -> ok end,
                       too_small),
    validate_by_option(maximum, Descriptor, Path,
                       fun(Max) when is_integer(Max) -> Max end,
                       invalid_maximum,
                       fun(Max) when Value =< Max ->ok end,
                       too_big),
    validate_by_option(divisible_by, Descriptor, Path,
                       fun(Divider) when is_integer(Divider), Divider =/= 0 -> Divider end,
                       invalid_divisible_by,
                       fun(Divider) when Value rem Divider =:= 0 -> ok end,
                       not_divisible),
    validate_by_option(format, Descriptor, Path,
                       fun(int8) -> fun matcher_lib:is_int8/1;
                          (int16) -> fun matcher_lib:is_int16/1;
                          (int32) -> fun matcher_lib:is_int32/1;
                          (int64) -> fun matcher_lib:is_int64/1;
                          (uint8) -> fun matcher_lib:is_uint8/1;
                          (uint16) -> fun matcher_lib:is_uint16/1;
                          (uint32) -> fun matcher_lib:is_uint32/1;
                          (uint64) -> fun matcher_lib:is_uint64/1
                       end,
                       unknown_format,
                       fun(ValidateFun) -> true = ValidateFun(Value) end,
                       incorrect_format),
    Value.

validate_number(#{type := number} = Descriptor, Value, _Options, Path, _Matchers) ->
    is_number(Value) orelse throw_error(not_valid, Path, wrong_type),
    validate_by_options([{minimum, fun(Min) when is_number(Min) -> Min end, invalid_minimum},
                         {exclusive_minimum, false, fun(ExclMin) when is_boolean(ExclMin) -> ExclMin end, invalid_exclusive_minimum}],
                        fun([{ok, Min}, {ok, false}]) -> Value >= Min;
                           ([{ok, Min}, {ok, true}]) -> Value > Min;
                           ([none, _]) -> true
                        end,
                        too_small,
                        Descriptor, Path),
    validate_by_options([{maximum, fun(Max) when is_number(Max) -> Max end, invalid_maximum},
                         {exclusive_maximum, false, fun(ExclMax) when is_boolean(ExclMax) -> ExclMax end, invalid_exclusive_maximum}],
                        fun([{ok, Max}, {ok, false}]) -> Value =< Max;
                           ([{ok, Max}, {ok, true}]) -> Value < Max;
                           ([none, _]) -> true
                        end,
                        too_big,
                        Descriptor, Path),
    Value.

validate_decimal_binary(#{type := decimal_binary} = Descriptor, Value, _Options, Path, _Matchers) ->
    is_binary(Value) orelse throw_error(not_valid, Path, wrong_type),
    ValueDec = try decimal:from_binary(Value) catch _:_ -> throw_error(not_valid, Path, not_decimal) end,
    validate_by_option(minimum, Descriptor, Path,
                       fun(Min) when is_binary(Min) -> decimal:from_binary(Min) end,
                       invalid_minimum,
                       fun(MinDec) -> true = decimal:cmp(MinDec, ValueDec) =/= 1 end,
                       too_small),
    validate_by_option(maximum, Descriptor, Path,
                       fun(Max) when is_binary(Max) -> decimal:from_binary(Max) end,
                       invalid_maximum,
                       fun(MaxDec) -> true = decimal:cmp(MaxDec, ValueDec) =/= -1 end,
                       too_big),
    validate_by_option(precision, Descriptor, Path,
                       fun(Precision) when is_integer(Precision) -> Precision end,
                       invalid_precision,
                       fun(Precision) ->
                               {_, ValuePrec} = decimal:normalize(ValueDec),
                               true = ValuePrec >= -Precision
                       end,
                       incorrect_precision),
    Value.

validate_boolean(#{type := boolean} = _Descriptor, Value, _Options, Path, _Matchers) ->
    is_boolean(Value) orelse throw_error(not_valid, Path, wrong_type),
    Value.

validate_term(#{type := term} = Descriptor, Value, _ExtraTypes, Path, _Matchers) ->
    validate_by_option(
      validator, Descriptor, Path,
      fun(ValidatorFun) when is_function(ValidatorFun, 1) -> ValidatorFun end,
      invalid_validator,
      fun(ValidatorFun) -> true = ValidatorFun(Value) end,
      not_match),
    Value.

validate_custom(#{type := custom} = Descriptor, Value, Options, Path, Matchers) ->
    Name = try maps:get(name, Descriptor) catch _:_ -> throw_error(bad_descriptor, Path, no_name) end,
    ExtraTypes = maps:get(extra_custom_types, Options, #{}),
    validate_by_option(
      custom_types, #{}, Descriptor, Path,
      fun(CTypes) when is_map(CTypes) ->
              AllTypes = maps:merge(CTypes, ExtraTypes),
              true = maps:is_key(Name, AllTypes),
              AllTypes
      end,
      invalid_custom_types,
      fun(AllTypes) ->
              TypeDescriptor = maps:get(Name, AllTypes),
              validate_item(TypeDescriptor, Value, Options#{extra_custom_types => AllTypes}, Path, Matchers)
      end,
      not_match). % In this case the error will not be used

validate_tuple(#{type := tuple} = Descriptor, Value, Options, Path, Matchers) ->
    is_tuple(Value) orelse throw_error(not_valid, Path, wrong_type),
    validate_by_option2(
      schema, Descriptor, Path,
      fun(Schema) when is_list(Schema) -> Schema end,
      invalid_schema,
      fun(Schema) when size(Value) =:= length(Schema) ->
              list_to_tuple(
                lists_map_pos(fun(Pos, ElementDescriptor) ->
                                      Element = element(Pos, Value),
                                      validate_item(ElementDescriptor, Element, Options, [{te, Pos} | Path], Matchers)
                              end, Schema))
      end,
      wrong_size,
      Value).

validate_list(#{type := list} = Descriptor, Value, Options, Path, Matchers) ->
    is_list(Value) orelse throw_error(not_valid, Path, wrong_type),
    validate_by_option(
      min_length, Descriptor, Path,
      fun(MinLength) when is_integer(MinLength), MinLength >= 0 -> MinLength end,
      invalid_min_length,
      fun(MinLength) when length(Value) >= MinLength -> true end,
      too_short),
    validate_by_option(
      max_length, Descriptor, Path,
      fun(MaxLength) when is_integer(MaxLength), MaxLength >= 0 -> MaxLength end,
      invalid_max_length,
      fun(MaxLength) when length(Value) =< MaxLength -> true end,
      too_long),
    validate_by_option2(
      schema, Descriptor, Path,
      fun(Schema) when is_list(Schema) -> Schema end,
      invalid_schema,
      fun(Schema) when Schema == [] ->
              [] = Value;
         (Schema) ->
              SchemaLength = length(Schema),
              LastDescriptor = lists:last(Schema),
              lists_map_pos(fun(Pos, Item) ->
                                    ItemDescriptor = if Pos < SchemaLength -> lists:nth(Pos, Schema);
                                                        true -> LastDescriptor
                                                     end,
                                    validate_item(ItemDescriptor, Item, Options, [{li, Pos} | Path], Matchers)
                            end, Value)
      end,
      too_long,
      Value).


validate_map(#{type := map} = Descriptor, Value, Options, Path, Matchers) ->
    is_map(Value) orelse throw_error(not_valid, Path, wrong_type),
    {SchemaIsNotDefined, SchemaKeys} =
        case maps:find(schema, Descriptor) of
            {ok, S} when is_map(S) -> {false, maps:keys(S)};
            {ok, _} -> throw_error(bad_descriptor, Path, invalid_schema);
            error -> {true, []}
        end,
    ExtraMap = maps:without(SchemaKeys, Value),
    MainMap = maps:with(SchemaKeys, Value),
    ExtraKeyEnabled = case maps:find(allow_extra_keys, Descriptor) of
                          {ok, AllowExtraKeys} when is_boolean(AllowExtraKeys) ->
                              AllowExtraKeys;
                          {ok, _} ->
                              throw_error(bad_descriptor, Path, invalid_allow_extra_keys);
                          error ->
                              maps:is_key(extra_keys_descriptor, Descriptor) orelse
                                  maps:is_key(extra_values_descriptor, Descriptor) orelse
                                  SchemaIsNotDefined
                      end,
    ExtraKeysNum = maps:size(ExtraMap),
    (ExtraKeysNum > 0 andalso not ExtraKeyEnabled) andalso
        throw_error(not_valid, [{mk, hd(maps:keys(ExtraMap))}|Path], extra_keys_not_allowed),
    validate_by_option(
      min_extra_keys, Descriptor, Path,
      fun(MinSize) when is_integer(MinSize), MinSize >= 0 -> MinSize end,
      invalid_min_extra_keys,
      fun(MinSize) when ExtraKeysNum >= MinSize -> true end,
      not_enough_extra_keys),
    validate_by_option(
      max_extra_keys, Descriptor, Path,
      fun(MaxSize) when is_integer(MaxSize), MaxSize >= 0 -> MaxSize end,
      invalid_max_extra_keys,
      fun(MaxSize) when ExtraKeysNum =< MaxSize -> true end,
      too_much_extra_keys),
    validate_by_option(
      extra_keys_descriptor, Descriptor, Path,
      fun(KDescr) when is_map(KDescr) -> KDescr end,
      invalid_extra_keys_descriptor,
      fun(KDescr) ->
              maps:map(fun(Key, _KeyValue) ->
                               validate_item(KDescr, Key, Options, [{mk, Key} | Path], Matchers)
                       end, ExtraMap),
              true
      end,
      not_match), % In this case the error will not be used
    R1 = validate_by_option2(
      extra_values_descriptor, Descriptor, Path,
      fun(VDescr) when is_map(VDescr) -> VDescr end,
      invalid_extra_values_descriptor,
      fun(VDescr) ->
              maps:map(fun(Key, KeyValue) ->
                               validate_item(VDescr, KeyValue, Options, [{mv, Key} | Path], Matchers)
                       end, ExtraMap)
      end,
      not_match, % In this case the error will not be used
      ExtraMap),
    R2 = validate_by_option2(
      schema, Descriptor, Path,
      fun(Schema) when is_map(Schema) -> Schema end,
      invalid_schema,
      fun(Schema) ->
              % Check missed
              maps:map(fun(Key, MapKeySpec) ->
                               Optional = maps:get(optional, MapKeySpec, false),
                               is_boolean(Optional) orelse
                                   throw_error(bad_descriptor, [{mk, Key} | Path], invalid_optional),
                               maps:is_key(Key, Value) orelse Optional orelse
                                   throw_error(not_valid, [{mk, Key} | Path], key_missed)
                       end, Schema),
              % Check conflicts
              maps:map(fun(Key, MapKeySpec) ->
                               case maps:find(conflicts, MapKeySpec) of
                                   {ok, List} when is_list(List) ->
                                       maps:size(maps:with(List, Value)) == 0 orelse
                                           throw_error(not_valid, [{mk, Key} | Path], conflicts);
                                   {ok, _} ->
                                       throw_error(bad_descriptor, [{mk, Key} | Path], invalid_conflicts);
                                   error -> ok
                               end
                       end, Schema),
              % Check required
              maps:map(fun(Key, MapKeySpec) ->
                               case maps:find(requires, MapKeySpec) of
                                   {ok, List}  when is_list(List) ->
                                       lists:all(fun(K) -> maps:is_key(K, Value) end, List) orelse
                                           throw_error(not_valid, [{mk, Key} | Path], missing_required);
                                   {ok, _} ->
                                       throw_error(bad_descriptor, [{mk, Key} | Path], invalid_requires);
                                   error -> ok
                               end
                       end, Schema),
              % Validate values
              maps:fold(fun(Key, MapKeySpec, Acc) ->
                                case maps:find(Key, MainMap) of
                                    {ok, KeyValue} ->
                                        Acc#{Key => validate_map_key(Key, KeyValue, MapKeySpec, Options, Path, Matchers)};
                                    error ->
                                        case maps:find(default, MapKeySpec) of
                                            {ok, DefaultValue} -> Acc#{Key => DefaultValue};
                                            error -> Acc
                                        end
                                end
                       end, #{}, Schema)
      end,
      unexpected,
      MainMap),
    maps:merge(R1, R2).

validate_alt(#{type := alt} = Descriptor, Value, Options, Path, Matchers) ->
    maps:is_key(schema, Descriptor) orelse throw_error(bad_descriptor, Path, no_schema),
    validate_by_option(
      schema, Descriptor, Path,
      fun(Schema) when is_list(Schema), length(Schema) > 0 -> Schema end,
      invalid_schema,
      fun(Schema) ->
              case lists:search(fun({PreDescriptor, _}) ->
                                        try
                                            validate_item(PreDescriptor, Value, Options, Path, Matchers),
                                            true
                                        catch
                                            throw:{bad_descriptor, _, _} = E -> throw(E);
                                            _:_ -> false
                                        end;
                                   (_) ->
                                        throw_error(bad_descriptor, Path, invalid_schema)
                                end, Schema) of
                  {value, {_, AltDescriptor}} ->
                      validate_item(AltDescriptor, Value, Options, Path, Matchers);
                  false ->
                      throw_error(not_valid, Path, alternatives_not_match)
              end
      end,
      invalid_schema). % The error will not be used in this case

validate_alt_simple(#{type := alt_simple} = Descriptor, Value, Options, Path, Matchers) ->
    maps:is_key(schema, Descriptor) orelse throw_error(bad_descriptor, Path, no_schema),
    validate_by_option(
      schema, Descriptor, Path,
      fun(Schema) when is_list(Schema), length(Schema) > 0 -> Schema end,
      invalid_schema,
      fun(Schema) ->
              % This is just a lookup for a first positive validation result
              try lists:foreach(fun(AltDescriptor) ->
                                        try validate_item(AltDescriptor, Value, Options, Path, Matchers) of
                                            Res -> throw({alt_found, Res})
                                        catch
                                            throw:{bad_descriptor, _, _} = E -> throw(E);
                                            throw:{not_valid, _, _} -> ok
                                        end
                                end, Schema) of
                  ok ->
                      throw_error(not_valid, Path, alternatives_not_match)
              catch
                  throw:{alt_found, Res} ->
                      Res
              end
      end,
      not_match). % The error will not be used in this case



-spec validate_map_key(Key::term(), Value::term(), MapKeySpec::map(), Options::map(), path(), matchers()) -> OutValue :: term().
validate_map_key(Key, Value, MapKeySpec, Options, Path, Matchers) ->
    case maps:find(descriptor, MapKeySpec) of
        {ok, Descriptor} ->
            validate_item(Descriptor, Value, Options, [{mv, Key} | Path], Matchers);
        error ->
            Value
    end.


-spec validate_by_option(OptionName, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError) -> true when
          OptionName :: any(), Descriptor :: descriptor(), Path :: path(),
          OptionPrepareFun :: fun((OptionValue :: any()) -> PreparedOptionValue :: any() | no_return()),
          OptionError :: any(),
          ValidationFun :: fun((PreparedOptionValue :: any()) -> boolean() | no_return()),
          ValidationError :: any() .
validate_by_option(OptionName, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError) ->
    validate_by_option2(OptionName, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError, ok).


-spec validate_by_option(OptionName, Default, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError) -> any() when
          OptionName :: any(), Default :: any(), Descriptor :: descriptor(), Path :: path(),
          OptionPrepareFun :: fun((OptionValue :: any()) -> PreparedOptionValue :: any() | no_return()),
          OptionError :: any(),
          ValidationFun :: fun((PreparedOptionValue :: any()) -> boolean() | no_return()),
          ValidationError :: any() .
validate_by_option(OptionName, Default, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError) ->
    Option = maps:get(OptionName, Descriptor, Default),
    PreparedOption = try OptionPrepareFun(Option) catch _C:_E -> throw_error(bad_descriptor, Path, OptionError) end,
    try ValidationFun(PreparedOption)
    catch
        throw:Error -> throw(Error);
        _:_ -> throw_error(not_valid, Path, ValidationError)
    end.


-spec validate_by_option2(OptionName, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError, Value) -> any() when
          OptionName :: any(), Descriptor :: descriptor(), Path :: path(),
          OptionPrepareFun :: fun((OptionValue :: any()) -> PreparedOptionValue :: any() | no_return()),
          OptionError :: any(),
          ValidationFun :: fun((PreparedOptionValue :: any()) -> boolean() | no_return()),
          ValidationError :: any(),
          Value :: any() .
validate_by_option2(OptionName, Descriptor, Path, OptionPrepareFun, OptionError, ValidationFun, ValidationError, Value) ->
    case maps:find(OptionName, Descriptor) of
        {ok, Option} ->
            PreparedOption = try OptionPrepareFun(Option) catch _:_ -> throw_error(bad_descriptor, Path, OptionError) end,
            try ValidationFun(PreparedOption)
            catch
                throw:Error -> throw(Error);
                _:_ -> throw_error(not_valid, Path, ValidationError)
            end;
        error ->
            Value
    end.



-spec validate_by_options(OptionsList, ValidationFun, ValidationError, Descriptor, Path) -> true when
          OptionsList :: [{OptionName, OptionPrepareFun, OptionError} | {OptionName, Default, OptionPrepareFun, OptionError}],
          OptionName :: any(), Default :: any(), Descriptor :: descriptor(), Path :: path(),
          OptionPrepareFun :: fun((OptionValue :: any()) -> PreparedOptionValue :: any() | no_return()),
          OptionError :: any(),
          ValidationFun :: fun((PreparedOptions :: list({ok, any()} | none)) -> boolean() | no_return()),
          ValidationError :: any() .
validate_by_options(OptionsList, ValidationFun, ValidationError, Descriptor, Path) ->
    Options = lists:map(
                fun({OptionName, OptionPrepareFun, OptionError}) ->
                        case maps:find(OptionName, Descriptor) of
                            {ok, Option} ->
                                PreparedOption = try OptionPrepareFun(Option) catch _:_ -> throw_error(bad_descriptor, Path, OptionError) end,
                                {ok, PreparedOption};
                            error ->
                                none
                        end;
                   ({OptionName, Default, OptionPrepareFun, OptionError}) ->
                        Option = maps:get(OptionName, Descriptor, Default),
                        PreparedOption = try OptionPrepareFun(Option) catch _:_ -> throw_error(bad_descriptor, Path, OptionError) end,
                        {ok, PreparedOption}
                end, OptionsList),
    try true = ValidationFun(Options)
    catch
        throw:Error -> throw(Error);
        _:_ -> throw_error(not_valid, Path, ValidationError)
    end.


-spec lists_map_pos(fun((Pos, ValueIn) -> ValueOut), ListIn) -> ListOut when
          Pos :: pos_integer(), ValueIn :: any(), ValueOut :: any(), ListIn :: list(), ListOut :: list().
lists_map_pos(_Fun, []) -> [];

lists_map_pos(Fun, List) ->
    lists:map(fun({Pos, Item}) -> Fun(Pos, Item) end, lists:zip(lists:seq(1, length(List)), List)).


throw_error(Type, Path, Reason) ->
    throw({Type, Path, Reason}).
