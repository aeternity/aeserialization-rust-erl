use rustler::types::binary::Binary;
use rustler::error::Error;
use rustler::*;
use crate::binaries::ErlBin;

use aeser::*;

#[rustler::nif]
pub fn rlp_encode<'a>(term: Term<'a>) -> NifResult<ErlBin> {
    let rlp: rlp::RlpItem = term.decode()?;
    let out = rlp.serialize();
    Ok(ErlBin::new(&out))
}

#[rustler::nif]
pub fn rlp_decode(bin: Binary) -> NifResult<rlp::RlpItem> {
    let rlp = aeser::rlp::RlpItem::deserialize(bin.as_slice());
    rlp.map_err(move |e| Error::RaiseTerm(Box::new(e)))
}

#[rustler::nif]
pub fn rlp_decode_one(bin: Binary) -> NifResult<(rlp::RlpItem, ErlBin)> {
    let decoded = aeser::rlp::RlpItem::try_deserialize(bin.as_slice());
    let (rlp, rest) = decoded.map_err(move |e| Error::RaiseTerm(Box::new(e)))?;
    Ok((rlp, ErlBin::new(&rest)))
}


#[rustler::nif]
pub fn id_encode(id: id::Id) -> NifResult<ErlBin> {
    Ok(ErlBin::new(&id.serialize()))
}

#[rustler::nif]
pub fn id_decode(bin: ErlBin) -> NifResult<id::Id> {
    id::Id::deserialize(&bin.bytes).map_err(|e| Error::RaiseTerm(Box::new(e)))
}

#[rustler::nif]
pub fn api_encoder_encode_data(t: api_encoder::KnownType, bin: ErlBin) -> NifResult<ErlBin> {
    let encoded = api_encoder::encode_data(t, &bin.bytes);
    Ok(ErlBin::new(&encoded))
}

#[rustler::nif]
pub fn api_encoder_encode_id(id: id::Id) -> NifResult<ErlBin> {
    let encoded = api_encoder::encode_id(&id);
    Ok(ErlBin::new(&encoded))
}


#[rustler::nif]
pub fn api_encoder_decode(data: ErlBin) -> NifResult<(api_encoder::KnownType, ErlBin)> {
    let (tp, decoded) = api_encoder::decode(&data.bytes)
        .map_err(|e| Error::RaiseTerm(Box::new(e)))?;
    Ok((tp, ErlBin::new(&decoded)))
}

#[rustler::nif]
pub fn api_encoder_safe_decode(data: ErlBin) -> NifResult<(api_encoder::KnownType, ErlBin)> {
    let (tp, decoded) = api_encoder::decode(&data.bytes)
        .map_err(|e| Error::Term(Box::new(e)))?;
    Ok((tp, ErlBin::new(&decoded)))
}

#[rustler::nif]
pub fn api_encoder_decode_id(allowed_types: Vec<api_encoder::KnownType>, data: ErlBin) -> NifResult<id::Id> {
    let decoded = api_encoder::decode_id(&allowed_types, data.bytes)
        .map_err(|e| Error::RaiseTerm(Box::new(e)))?;
    Ok(decoded)
}

#[rustler::nif]
pub fn api_encoder_decode_blockhash(data: ErlBin) -> NifResult<ErlBin> {
    let decoded = api_encoder::decode_blockhash(data.bytes)
        .map_err(|e| Error::RaiseTerm(Box::new(e)))?;
    Ok(ErlBin::new(&decoded))
}

#[rustler::nif]
pub fn api_encoder_byte_size_for_type(tp: api_encoder::KnownType) -> NifResult<Option<usize>> {
    Ok(tp.byte_size())
}

#[rustler::nif]
pub fn contract_code_serialize(code: contract_code::Code) -> NifResult<ErlBin> {
    Ok(ErlBin::new(&code.serialize()))
}

#[rustler::nif]
pub fn contract_code_deserialize(bin: ErlBin) -> NifResult<contract_code::Code> {
    Ok(contract_code::Code::deserialize(&bin.bytes)
        .map_err(|e| Error::RaiseTerm(Box::new(e)))?)
}
