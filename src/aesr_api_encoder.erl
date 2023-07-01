-module(aeser_api_encoder).

-export([encode/2,
         decode/1,
         safe_decode/2,
         byte_size_for_type/1]).

-export_type([encoded/0]).

-type known_type() :: key_block_hash
                    | micro_block_hash
                    | block_pof_hash
                    | block_tx_hash
                    | block_state_hash
                    | channel
                    | contract_bytearray
                    | contract_pubkey
                    | contract_store_key
                    | contract_store_value
                    | transaction
                    | tx_hash
                    | oracle_pubkey
                    | oracle_query
                    | oracle_query_id
                    | oracle_response
                    | account_pubkey
                    | signature
                    | name
                    | commitment
                    | peer_pubkey
                    | state
                    | poi
                    | state_trees
                    | call_state_tree
                    | bytearray.

-type extended_type() :: known_type() | block_hash | {id_hash, [known_type()]}.


-type payload() :: binary().
-type encoded() :: binary().

-spec encode(known_type(), payload() | aeser_id:id()) -> encoded().
encode(id_hash, Payload) ->
    aeser_nif:api_encoder_encode_id(Payload);
encode(Type, Payload) ->
    aeser_nif:api_encoder_encode_data(Type, Payload).

-spec decode(binary()) -> {known_type(), payload()}.
decode(Bin) ->
    aeser_nif:api_encoder_decode(Bin).

-spec safe_decode(extended_type(), encoded()) -> {'ok', payload() | aeser_id:id()}
                                                     | {'error', atom()}.
safe_decode({id_hash, AllowedTypes}, Enc) ->
    aeser_nif:api_encoder_decode_id(AllowedTypes, Enc);
safe_decode(block_hash, Enc) ->
    aeser_nif:api_encoder_decode_blockhash(Enc);
safe_decode(Type, Enc) ->
    try aeser_nif:api_encoder_safe_decode(Enc) of
        {Type, Dec} ->
            {ok, Dec};
        {error, Err} = E ->
            E;
        {_, _} ->
            {error, invalid_prefix}
    catch
        error:_ ->
            {error, invalid_encoding}
    end.

-spec byte_size_for_type(known_type()) -> non_neg_integer() | not_applicable.

byte_size_for_type(Type) ->
    aeser_nif:api_encoder_byte_size_for_type(Type).
