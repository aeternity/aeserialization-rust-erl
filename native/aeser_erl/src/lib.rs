pub mod aeser_rlp;

rustler::init!(
    "aeser_rlp",
    [
        aeser_rlp::encode,
        aeser_rlp::decode,
        aeser_rlp::decode_one
    ]
);
