mod erl;
pub mod aeser_nif;

use aeser_nif::*;

rustler::init!(
    "aeser_nif",
    [
        rlp_encode,
        rlp_decode,
        rlp_decode_one,
        id_encode,
        id_decode,
        api_encoder_encode_data,
        api_encoder_encode_id,
        api_encoder_decode,
        api_encoder_safe_decode,
        api_encoder_decode_id,
        api_encoder_decode_blockhash,
        api_encoder_byte_size_for_type,
        contract_code_serialize,
        contract_code_deserialize,
    ]
);
