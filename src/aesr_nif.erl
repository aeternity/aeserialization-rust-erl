-module(aesr_nif).

-export([rlp_encode/1,
         rlp_decode/1,
         rlp_decode_one/1
        ]).

-export([id_encode/1,
         id_decode/1
        ]).

-export([api_encoder_encode_data/2,
         api_encoder_encode_id/1,
         api_encoder_decode/1,
         api_encoder_safe_decode/1,
         api_encoder_decode_id/2,
         api_encoder_decode_blockhash/1,
         api_encoder_byte_size_for_type/1
        ]).

-export([contract_code_serialize/1,
         contract_code_deserialize/1
        ]).


-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

init() ->
    ?load_nif_from_crate(aesr_nif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% NIF
%%%===================================================================


id_encode(_Id) ->
    ?NOT_LOADED.

id_decode(_Bin) ->
   ?NOT_LOADED.

rlp_encode(_Data) ->
    ?NOT_LOADED.

rlp_decode(_Data) ->
    ?NOT_LOADED.

rlp_decode_one(_Data) ->
    ?NOT_LOADED.


api_encoder_encode_data(_Type, _Bin) ->
    ?NOT_LOADED.

api_encoder_encode_id(_Id) ->
    ?NOT_LOADED.

api_encoder_decode(_Data) ->
    ?NOT_LOADED.

api_encoder_safe_decode(_Data) ->
    ?NOT_LOADED.

api_encoder_decode_id(_AllowedTypes, _Data) ->
    ?NOT_LOADED.

api_encoder_decode_blockhash(_Data) ->
    ?NOT_LOADED.

api_encoder_byte_size_for_type(_Type) ->
    ?NOT_LOADED.

contract_code_serialize(_CodeMap) ->
    ?NOT_LOADED.

contract_code_deserialize(_Binary) ->
    ?NOT_LOADED.
