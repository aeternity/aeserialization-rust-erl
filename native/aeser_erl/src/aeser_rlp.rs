use rustler::env::Env;
use rustler::types::{atom::Atom, binary::{Binary, NewBinary}};
use rustler::error::Error;
use rustler::*;

use aeser::rlp::{RLPItem, DecodingErr};

fn decode_rlp(term: &Term) -> NifResult<RLPItem> {
        if term.is_binary() {
            Ok(RLPItem::ByteArray(
                term.decode_as_binary()?.as_slice().to_vec(),
            ))
        } else if term.is_list() {
            let list: Vec<Term> = term.decode()?;
            let rlps: NifResult<Vec<RLPItem>> = list.iter().map(|x| decode_rlp(x)).collect();
            Ok(RLPItem::List(rlps?))
        } else {
            Err(Error::BadArg)
        }
    }

fn encode_err<'a>(err: DecodingErr, env: Env<'a>) -> Error  {
    let err_term = match err {
        DecodingErr::Trailing {
            input: input,
            undecoded: undecoded,
            decoded: decoded
        } => {
            let header = Atom::from_str(env, "trailing").unwrap().to_term(env);
            let input_term: Binary<'a> = make_bin(env, &input);
            let undecoded_term: Binary<'a> = make_bin(env, &undecoded);
            let decoded_term: Binary<'a> = make_bin(env, &decoded.to_bytes());
            (input_term, undecoded_term, decoded_term).encode::<'a>(env)
        },
        DecodingErr::LeadingZerosInSize => {
            Atom::from_str(env, "trailing").unwrap().to_term(env)
        }
    };
    //Error::Term(Box::new(err_term)) // todo wtf
    Error::BadArg
}


fn make_bin<'a>(env: Env<'a>, data: &[u8]) -> Binary<'a> {
    let mut outbin = NewBinary::new(env, data.len());
    outbin.as_mut_slice().copy_from_slice(data);
    Binary::from(outbin)
}

#[rustler::nif]
fn encode<'a>(env: Env<'a>, term: Term<'a>) -> NifResult<Binary<'a>> {
    let rlp = decode_rlp(&term)?;
    let out = aeser::rlp::encode(&rlp);
    Ok(make_bin(env, &out))
}


#[rustler::nif]
fn decode<'a>(env: Env<'a>, term: Binary) -> NifResult<Binary<'a>> {
    let rlp = aeser::rlp::decode(term.as_slice()).map_err(|e| encode_err(e, env))?;
    let bytes = rlp.to_bytes();
    Ok(make_bin(env, &bytes))
}


#[rustler::nif]
fn decode_one<'a>(env: Env<'a>, term: Binary) -> NifResult<(Binary<'a>, Binary<'a>)> {
    let (rlp, rest) = aeser::rlp::try_decode(term.as_slice()).map_err(|e| encode_err(e, env))?;
    let rlp_bytes = rlp.to_bytes();
    Ok((make_bin(env, &rlp_bytes), make_bin(env, &rest)))
}
