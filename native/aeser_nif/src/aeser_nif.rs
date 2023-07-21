use rustler::error::Error;
use rustler::*;
use crate::erl::Erl;

use aeser::*;

#[rustler::nif]
pub fn rlp_encode<'a>(rlp: Erl<rlp::RlpItem>) -> NifResult<Erl<Bytes>> {
    let out = rlp.val.serialize();
    Ok(Erl{val: out})
}

#[rustler::nif]
pub fn rlp_decode(bin: Erl<Bytes>) -> NifResult<Erl<rlp::RlpItem>> {
    let rlp_e = rlp::RlpItem::deserialize(bin.val.as_slice());
    let rlp = rlp_e.map_err(move |e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok(Erl{val: rlp})
}

#[rustler::nif]
pub fn rlp_decode_one(bin: Erl<Bytes>) -> NifResult<(Erl<rlp::RlpItem>, Erl<Bytes>)> {
    let decoded = rlp::RlpItem::try_deserialize(bin.val.as_slice());
    let (rlp, rest) = decoded.map_err(move |e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok( (Erl{val: rlp}, Erl{val: rest.to_vec()}) )
}


#[rustler::nif]
pub fn id_encode(id: Erl<id::Id>) -> NifResult<Erl<Bytes>> {
    Ok(Erl{val: id.val.serialize()})
}

#[rustler::nif]
pub fn id_decode(bin: Erl<Bytes>) -> NifResult<Erl<id::Id>> {
    let id = id::Id::deserialize(&bin.val).map_err(|e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok(Erl{val: id})
}

#[rustler::nif]
pub fn api_encoder_encode_data(t: Erl<api_encoder::KnownType>, bin: Erl<Bytes>) -> NifResult<Erl<Bytes>> {
    let encoded = api_encoder::encode_data(t.val, &bin.val);
    Ok(Erl{val: encoded.into_bytes()})
}

#[rustler::nif]
pub fn api_encoder_encode_id(id: Erl<id::Id>) -> NifResult<Erl<Bytes>> {
    let encoded = api_encoder::encode_id(&id.val);
    Ok(Erl{val: encoded.as_bytes().to_vec()})
}


#[rustler::nif]
pub fn api_encoder_decode(data: String) -> NifResult<(Erl<api_encoder::KnownType>, Erl<Bytes>)> {
    let (tp, decoded) = api_encoder::decode(&data)
        .map_err(|e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok((Erl{val: tp}, Erl{val: decoded}))
}

#[rustler::nif]
pub fn api_encoder_safe_decode(data: String) -> NifResult<(Erl<api_encoder::KnownType>, Erl<Bytes>)> {
    let (tp, decoded) = api_encoder::decode(&data)
        .map_err(|e| Error::Term(Box::new(Erl{val: e})))?;
    Ok((Erl{val: tp}, Erl{val: decoded}))
}

#[rustler::nif]
pub fn api_encoder_decode_id(allowed_types: Vec<Erl<api_encoder::KnownType>>, data: String) -> NifResult<Erl<id::Id>> {
    let decoded = api_encoder::decode_id(&allowed_types.iter().map(|e| e.val).collect::<Vec<_>>(), &data)
        .map_err(|e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok(Erl{val: decoded})
}

#[rustler::nif]
pub fn api_encoder_decode_blockhash(data: String) -> NifResult<Erl<Bytes>> {
    let decoded = api_encoder::decode_blockhash(&data)
        .map_err(|e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok(Erl{val: decoded})
}

#[rustler::nif]
pub fn api_encoder_byte_size_for_type(tp: Erl<api_encoder::KnownType>) -> NifResult<Option<usize>> {
    Ok(tp.val.byte_size())
}

#[rustler::nif]
pub fn contract_code_serialize(code: Erl<contract_code::Code>) -> NifResult<Erl<Bytes>> {
    Ok(Erl{val: code.val.serialize()})
}

#[rustler::nif]
pub fn contract_code_deserialize(bin: Erl<Bytes>) -> NifResult<Erl<contract_code::Code>> {
    let code = contract_code::Code::deserialize(&bin.val)
        .map_err(|e| Error::RaiseTerm(Box::new(Erl{val: e})))?;
    Ok(Erl{val: code})
}
