mod binaries;
pub mod aeser_nif;

use aeser_nif::*;

rustler::init!(
    "aeser_nif",
    [
        rlp_encode,
        rlp_decode,
        rlp_decode_one,
        id_encode,
        id_decode
    ]
);
