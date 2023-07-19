use aeser::*;
use rustler::*;

pub struct Erl<T> {
    pub val: T,
}

pub fn to_term<'a, 'e, T>(env: Env<'a>, val: &'e T) -> Term<'a>
where Erl<&'e T>: Encoder {
    Erl{val}.encode(env)
}

pub fn from_term<'a, T>(term: Term<'a>) -> NifResult<T>
where Erl<T>: Decoder<'a> {
    Ok(<Erl<T> as Decoder<'a>>::decode(term)?.val)
}

impl Encoder for Erl<Bytes> {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut bin = NewBinary::new(env, self.val.len());
        bin.as_mut_slice().copy_from_slice(&self.val);
        Term::from(bin)
    }
}

impl<'a> Decoder<'a> for Erl<Bytes> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if !term.is_binary() {
            Err(Error::BadArg)?;
        }

        let bin = Binary::from_term(term)?;
        let data = bin.as_slice();

        Ok(Erl{val: data.to_vec()})
    }
}

impl Encoder for Erl<rlp::RlpItem> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self.val {
            rlp::RlpItem::ByteArray(bytes) => to_term(env, &bytes),
            rlp::RlpItem::List(rlps) => rlps.iter().rfold(Term::list_new_empty(env), |acc, el| {
                acc.list_prepend(to_term(env, *el))
            }),
        }
    }
}

impl<'a> Decoder<'a> for Erl<rlp::RlpItem> {
    fn decode(term: Term) -> NifResult<Self> {
        if term.is_binary() {
            Ok(erl(rlp::RlpItem::ByteArray(
                term.decode_as_binary()?.as_slice().to_vec(),
            )))
        } else if term.is_list() {
            let list: Vec<Term> = from_term(term)?;
            let rlps: NifResult<Vec<rlp::RlpItem>> = list.iter().map(from_term).collect();
            Ok(erl(rlp::RlpItem::List(rlps?)))
        } else {
            Err(Error::BadArg)
        }
    }
}

mod encoding_err {
    rustler::atoms! {
        trailing,
        leading_zeros_in_size,
        size_overflow,
        empty,
    }
}

impl Encoder for Erl<rlp::DecodingErr> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self.val {
            rlp::DecodingErr::Trailing {
                input,
                undecoded,
                decoded,
            } => {
                (encoding_err::trailing(), input, undecoded, decoded).encode(env)
            }
            rlp::DecodingErr::LeadingZerosInSize { position } => {
                (encoding_err::leading_zeros_in_size(), position).encode(env)
            }
            rlp::DecodingErr::SizeOverflow {
                position,
                expected,
                actual,
            } => {
                (encoding_err::size_overflow(), position, expected, actual).encode(env)
            }
            rlp::DecodingErr::Empty => encoding_err::empty(),
        }
    }
}

mod fields {
    rustler::atoms! {
        type_hash,
        name,
        payable,
        arg_type,
        out_type,
        byte_code,
        source_hash,
        compiler_version,
        type_info,
    }
}

impl Encoder for Erl<contract_code::TypeInfo> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        (
            erl(&self.val.type_hash),
            erl(&self.val.name),
            self.val.payable.encode(env),
            erl(&self.val.arg_type),
            erl(&self.val.out_type),
        ).encode(env)
    }
}

impl<'a> Decoder<'a> for Erl<contract_code::TypeInfo> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let (type_hash, name, payable_, arg_type, out_type) =
            <(Term<'a>, Term<'a>, bool, Term<'a>, Term<'a>) as Decoder<'a>>::decode(term)?;
        let type_info = TypeInfo {
            type_hash: open_bin(type_hash)?,
            name: open_bin(name)?,
            payable: payable_,
            arg_type: open_bin(arg_type)?,
            out_type: open_bin(out_type)?,
        };
        Ok(type_info)
    }
}

impl Encoder for Erl<contract_code::Code> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        Term::map_from_pairs(
            env,
            &[
                (fields::byte_code(), erl(&self.val.byte_code)),
                (fields::payable(), self.val.payable.encode(env)),
                (fields::source_hash(), erl(&self.val.source_hash)),
                (fields::compiler_version(), erl(&self.compiler_version)),
                (fields::type_info(), self.type_info.encode(env)),
            ],
        )
        .expect("Failed creating an Erlang map")
    }
}

impl<'a> Decoder<'a> for Erl<contract_code::Code> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if !term.map_get(fields::type_info())?.is_empty_list() {
            Err(Error::BadArg)?;
        }

        let code = Code {
            byte_code: open_bin(term.map_get(fields::byte_code())?)?,
            payable: term.map_get(fields::payable())?.decode()?,
            source_hash: open_bin(term.map_get(fields::source_hash())?)?,
            compiler_version: open_bin(term.map_get(fields::compiler_version())?)?,
            type_info: term.map_get(fields::type_info())?.decode()?,
        };
        Ok(code)
    }
}
