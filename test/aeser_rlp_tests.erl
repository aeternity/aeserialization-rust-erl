%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Tests for Recursive Length Prefix
%%% @end
%%%-------------------------------------------------------------------

-module(aeser_rlp_tests).

-include_lib("eunit/include/eunit.hrl").

-define(UNTAGGED_SIZE_LIMIT , 55).
-define(UNTAGGED_LIMIT      , 127).
-define(BYTE_ARRAY_OFFSET   , 128).
-define(LIST_OFFSET         , 192).

-define(TEST_MODULE, aeser_rlp).

rlp_one_byte_test() ->
    B = <<42>>,
    B = ?TEST_MODULE:encode(B),
    B = ?TEST_MODULE:decode(B).

rlp_another_one_byte_test() ->
    B = <<127>>,
    B = ?TEST_MODULE:encode(B),
    B = ?TEST_MODULE:decode(B).

rlp_zero_bytes_test() ->
    B = <<>>,
    S = ?BYTE_ARRAY_OFFSET + 0,
    <<S, B/binary>> = ?TEST_MODULE:encode(B).

rlp_two_bytes_test() ->
    B = <<128>>,
    S = ?BYTE_ARRAY_OFFSET + 1,
    <<S, B/binary>> = ?TEST_MODULE:encode(B).

rlp_one_byte_size_bytes_test() ->
    L = 55,
    S = ?BYTE_ARRAY_OFFSET + L,
    X = << <<X>> || X <- lists:seq(1,L)>>,
    E = <<S, X/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_tagged_size_one_byte_bytes_test() ->
    L = 56,
    Tag = ?BYTE_ARRAY_OFFSET + ?UNTAGGED_SIZE_LIMIT + 1,
    X = list_to_binary(lists:duplicate(L, 42)),
    S = byte_size(X),
    E = <<Tag, S, X/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_tagged_size_two_bytes_bytes_test() ->
    L = 256,
    SizeSize = 2,
    Tag = ?BYTE_ARRAY_OFFSET + ?UNTAGGED_SIZE_LIMIT + SizeSize,
    X = list_to_binary(lists:duplicate(L, 42)),
    S = byte_size(X),
    E = <<Tag, S:SizeSize/unit:8, X/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_zero_bytes_list_test() ->
    L = 0,
    Tag = ?LIST_OFFSET + L,
    X = [],
    E = <<Tag>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_one_byte_list_test() ->
    L = 1,
    Tag = ?LIST_OFFSET + L,
    X = lists:duplicate(L, <<42>>),
    E = <<Tag, 42>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_byte_array_list_test() ->
    L = 55,
    Tag = ?LIST_OFFSET +  L,
    X = lists:duplicate(L, <<42>>),
    Y = list_to_binary(X),
    E = <<Tag, Y/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_byte_array_tagged_size_one_byte_list_test() ->
    L = 56,
    SizeSize = 1,
    Tag = ?LIST_OFFSET + ?UNTAGGED_SIZE_LIMIT + SizeSize,
    X = lists:duplicate(L, <<42>>),
    Y = list_to_binary(X),
    S = byte_size(Y),
    E = <<Tag, S:SizeSize/unit:8, Y/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

rlp_byte_array_tagged_size_two_bytes_list_test() ->
    L = 256,
    SizeSize = 2,
    Tag = ?LIST_OFFSET + ?UNTAGGED_SIZE_LIMIT + SizeSize,
    X = lists:duplicate(L, <<42>>),
    Y = list_to_binary(X),
    S = byte_size(Y),
    E = <<Tag, S:SizeSize/unit:8, Y/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E).

illegal_size_encoding_list_test() ->
    %% Ensure we start with somehting legal.
    L = 56,
    SizeSize = 1,
    Tag = ?LIST_OFFSET + ?UNTAGGED_SIZE_LIMIT + SizeSize,
    X = lists:duplicate(L, <<42>>),
    Y = list_to_binary(X),
    S = byte_size(Y),
    E = <<Tag, S:SizeSize/unit:8, Y/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E),

    %% Add leading zeroes to the size field.
    E1 = <<(Tag + 1), 0, S:SizeSize/unit:8, Y/binary>>,
    ?assertError({leading_zeros_in_size, _}, ?TEST_MODULE:decode(E1)).

illegal_size_encoding_byte_array_test() ->
    %% Ensure we start with somehting legal.
    L = 256,
    SizeSize = 2,
    Tag = ?BYTE_ARRAY_OFFSET + ?UNTAGGED_SIZE_LIMIT + SizeSize,
    X = list_to_binary(lists:duplicate(L, 42)),
    S = byte_size(X),
    E = <<Tag, S:SizeSize/unit:8, X/binary>> = ?TEST_MODULE:encode(X),
    X = ?TEST_MODULE:decode(E),

    %% Add leading zeroes to the size field.
    E1 = <<(Tag + 1), 0, S:SizeSize/unit:8, X/binary>>,
    ?assertError({leading_zeros_in_size, _}, ?TEST_MODULE:decode(E1)).
