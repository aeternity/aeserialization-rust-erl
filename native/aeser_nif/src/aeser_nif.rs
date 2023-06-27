use rustler::types::binary::Binary;
use rustler::error::Error;
use rustler::*;
use crate::binaries::ErlBin;

use aeser::{id, rlp};

#[rustler::nif]
pub fn rlp_encode<'a>(term: Term<'a>) -> NifResult<ErlBin> {
    let rlp = term.decode()?;
    let out = aeser::rlp::encode(&rlp);
    Ok(ErlBin::new(&out))
}

#[rustler::nif]
pub fn rlp_decode(bin: Binary) -> NifResult<rlp::RlpItem> {
    let rlp = aeser::rlp::decode(bin.as_slice());
    rlp.map_err(move |e| Error::Term(Box::new(e)))
}

#[rustler::nif]
pub fn rlp_decode_one(bin: Binary) -> NifResult<(rlp::RlpItem, ErlBin)> {
    let decoded = aeser::rlp::try_decode(bin.as_slice());
    let (rlp, rest) = decoded.map_err(move |e| Error::Term(Box::new(e)))?;
    Ok((rlp, ErlBin::new(&rest)))
}


#[rustler::nif]
pub fn id_encode(id: id::Id) -> NifResult<ErlBin> {
    Ok(ErlBin::new(&id.encode()))
}

#[rustler::nif]
pub fn id_decode(bin: ErlBin) -> NifResult<id::Id> {
    id::Id::decode(&bin.bytes).map_err(|e| Error::Term(Box::new(e)))
}
