use rustler::env::{Env, SavedTerm};
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

fn encode_rlp<'a>(rlp: &RLPItem, env: Env<'a>) -> Term<'a> {
    match rlp {
        RLPItem::ByteArray(bytes) => {
            let bin = make_bin(env, bytes);
            bin.to_term(env)
        },
        RLPItem::List(rlps) => {
            rlps.iter().fold(Term::list_new_empty(env), |acc, el| {
                let encoded = encode_rlp(el, env);
                acc.list_prepend(encoded)
            })

        }

    }
}

fn encode_err<'a>(err: DecodingErr, env: Env<'a>) -> Error  {
    match err {
        DecodingErr::Trailing {
            input,
            undecoded,
            decoded
        } => {
            let header = Atom::from_str(env, "trailing").unwrap().to_term(env);
            let input_term: Binary = make_bin(env, &input);
            let undecoded_term: Binary = make_bin(env, &undecoded);
            let rlp_erl = encode_rlp(&decoded, env);
            let err_object = (header, input_term, undecoded_term, rlp_erl);
            let err_term = err_object.encode(env);
            // Error::RaiseTerm(Box::new(err_term))
            Error::Atom("trailing")
        },
        DecodingErr::SizeOverflow{expected, actual, position} => {
            let header = Atom::from_str(env, "size_overflow").unwrap().to_term(env);
            let err_object = (header, expected, actual, position);
            let err_term = err_object.encode(env);

            // Error::RaiseTerm(Box::new(err_term))
            Error::Atom("size_overflow")
        },
        DecodingErr::LeadingZerosInSize{position} => {
            Error::RaiseAtom("leading_zeros_in_size")
        },
        DecodingErr::Empty => {
            Error::RaiseAtom("empty")
        },
    }
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
fn decode<'a>(env: Env<'a>, term: Binary) -> NifResult<Term<'a>> {
    let rlp = aeser::rlp::decode(term.as_slice()).map_err(|e| encode_err(e, env))?;
    let rlp_erl = encode_rlp(&rlp, env);
    Ok(rlp_erl)
}


#[rustler::nif]
fn decode_one<'a>(env: Env<'a>, term: Binary) -> NifResult<(Term<'a>, Binary<'a>)> {
    let (rlp, rest) = aeser::rlp::try_decode(term.as_slice()).map_err(|e| encode_err(e, env))?;
    let rlp_erl = encode_rlp(&rlp, env);
    Ok((rlp_erl, make_bin(env, &rest)))
}
