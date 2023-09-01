use aeser::*;
use rustler::*;

pub struct Erl<T> {
    pub val: T,
}

trait MockEncoder {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a>;
}

trait MockDecoder<'a>: Sized + 'a {
    fn mock_decode(term: Term<'a>) -> NifResult<Self>;
}

impl<T> Encoder for Erl<T>
where
    T: MockEncoder,
{
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.val.mock_encode(env)
    }
}

impl<'a, T> Decoder<'a> for Erl<T>
where
    T: MockDecoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        Ok(Erl {
            val: <T as MockDecoder>::mock_decode(term)?,
        })
    }
}

impl<T> MockEncoder for Vec<T>
where
    T: MockEncoder,
{
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.iter()
            .map(|x| x.mock_encode(env))
            .collect::<Vec<_>>()
            .encode(env)
    }
}

impl<'a, T> MockDecoder<'a> for Vec<T>
where
    T: MockDecoder<'a>,
{
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        let iter: ListIterator = term.decode()?;
        let res: NifResult<Self> = iter.map(|x| MockDecoder::mock_decode(x)).collect();
        res
    }
}

impl MockEncoder for Bytes {
    fn mock_encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut bin = NewBinary::new(env, self.len());
        bin.as_mut_slice().copy_from_slice(self);
        Term::from(bin)
    }
}

impl<'a> MockDecoder<'a> for Bytes {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        if !term.is_binary() {
            Err(Error::BadArg)?;
        }

        let bin = Binary::from_term(term)?;
        let data = bin.as_slice();

        Ok(data.to_vec())
    }
}

impl MockEncoder for rlp::RlpItem {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            rlp::RlpItem::ByteArray(bytes) => bytes.mock_encode(env),
            rlp::RlpItem::List(rlps) => rlps.iter().rfold(Term::list_new_empty(env), |acc, el| {
                acc.list_prepend(el.mock_encode(env))
            }),
        }
    }
}

impl<'a> MockDecoder<'a> for rlp::RlpItem {
    fn mock_decode(term: Term) -> NifResult<rlp::RlpItem> {
        if term.is_binary() {
            Ok(rlp::RlpItem::ByteArray(
                term.decode_as_binary()?.as_slice().to_vec(),
            ))
        } else if term.is_list() {
            let list: Vec<Term> = term.decode()?;
            let rlps: NifResult<Vec<rlp::RlpItem>> =
                list.iter().map(|x| MockDecoder::mock_decode(*x)).collect();
            Ok(rlp::RlpItem::List(rlps?))
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

impl MockEncoder for rlp::DecodingErr {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            rlp::DecodingErr::Trailing {
                input,
                undecoded,
                decoded,
            } => (
                encoding_err::trailing(),
                input.mock_encode(env),
                undecoded.mock_encode(env),
                decoded.mock_encode(env),
            )
                .encode(env),
            rlp::DecodingErr::LeadingZerosInSize { position } => {
                (encoding_err::leading_zeros_in_size(), position).encode(env)
            }
            rlp::DecodingErr::SizeOverflow {
                position,
                expected,
                actual,
            } => (encoding_err::size_overflow(), position, expected, actual).encode(env),
            rlp::DecodingErr::Empty => encoding_err::empty().encode(env),
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

impl MockEncoder for contract_code::TypeInfo {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        (
            self.type_hash.mock_encode(env),
            self.name.mock_encode(env),
            self.payable.encode(env),
            self.arg_type.mock_encode(env),
            self.out_type.mock_encode(env),
        )
            .encode(env)
    }
}

impl<'a> MockDecoder<'a> for contract_code::TypeInfo {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        let (type_hash, name, payable_, arg_type, out_type) =
            <(Term<'a>, Term<'a>, bool, Term<'a>, Term<'a>) as Decoder<'a>>::decode(term)?;
        let type_info = contract_code::TypeInfo {
            type_hash: MockDecoder::mock_decode(type_hash)?,
            name: MockDecoder::mock_decode(name)?,
            payable: payable_,
            arg_type: MockDecoder::mock_decode(arg_type)?,
            out_type: MockDecoder::mock_decode(out_type)?,
        };
        Ok(type_info)
    }
}

impl MockEncoder for contract_code::Code {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        Term::map_from_pairs(
            env,
            &[
                (fields::byte_code(), self.byte_code.mock_encode(env)),
                (fields::payable(), self.payable.encode(env)),
                (fields::source_hash(), self.source_hash.mock_encode(env)),
                (
                    fields::compiler_version(),
                    self.compiler_version.mock_encode(env),
                ),
                (fields::type_info(), self.type_info.mock_encode(env)),
            ],
        )
        .expect("Failed creating an Erlang map")
    }
}

impl<'a> MockDecoder<'a> for contract_code::Code {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        if !term.map_get(fields::type_info())?.is_empty_list() {
            Err(Error::BadArg)?;
        }

        let code = contract_code::Code {
            byte_code: MockDecoder::mock_decode(term.map_get(fields::byte_code())?)?,
            payable: term.map_get(fields::payable())?.decode()?,
            source_hash: MockDecoder::mock_decode(term.map_get(fields::source_hash())?)?,
            compiler_version: MockDecoder::mock_decode(term.map_get(fields::compiler_version())?)?,
            type_info: MockDecoder::mock_decode(term.map_get(fields::type_info())?)?,
        };
        Ok(code)
    }
}

mod tag {
    rustler::atoms! {
        account,
        name,
        commitment,
        oracle,
        contract,
        channel,
    }
}

impl MockEncoder for id::Tag {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            id::Tag::Account => tag::account(),
            id::Tag::Name => tag::name(),
            id::Tag::Commitment => tag::commitment(),
            id::Tag::Oracle => tag::oracle(),
            id::Tag::Contract => tag::contract(),
            id::Tag::Channel => tag::channel(),
        }
        .encode(env)
    }
}

impl<'a> MockDecoder<'a> for id::Tag {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        match term.atom_to_string()?.as_str() {
            "account" => Ok(id::Tag::Account),
            "name" => Ok(id::Tag::Name),
            "commitment" => Ok(id::Tag::Commitment),
            "oracle" => Ok(id::Tag::Oracle),
            "contract" => Ok(id::Tag::Contract),
            "channel" => Ok(id::Tag::Channel),
            _ => Err(Error::BadArg),
        }
    }
}

impl MockEncoder for id::EncodedId {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        let mut bin = NewBinary::new(env, self.bytes.len());
        bin.as_mut_slice().copy_from_slice(&self.bytes);
        Binary::from(bin).to_term(env)
    }
}

impl<'a> MockDecoder<'a> for id::EncodedId {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        let bin = term.decode_as_binary()?;
        let bytes: &[u8; 32] = bin.as_slice().try_into().map_err(|_| Error::BadArg)?;

        Ok(id::EncodedId { bytes: *bytes })
    }
}

impl MockEncoder for id::Id {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        (
            Atom::from_str(env, "id").unwrap().encode(env),
            self.tag.mock_encode(env),
            self.val.mock_encode(env),
        )
            .encode(env)
    }
}

impl<'a> MockDecoder<'a> for id::Id {
    fn mock_decode(term: Term<'a>) -> NifResult<id::Id> {
        let tup = types::tuple::get_tuple(term)?;

        if tup.len() != 3 {
            Err(Error::BadArg)?;
        }

        if tup[0].atom_to_string()? != "id" {
            Err(Error::BadArg)?;
        }

        Ok(id::Id {
            tag: MockDecoder::mock_decode(tup[1])?,
            val: MockDecoder::mock_decode(tup[2])?,
        })
    }
}

mod known_type {
    rustler::atoms! {
        key_block_hash,
        micro_block_hash,
        block_pof_hash,
        block_tx_hash,
        block_state_hash,
        channel,
        contract_bytearray,
        contract_pubkey,
        contract_store_key,
        contract_store_value,
        transaction,
        tx_hash,
        oracle_pubkey,
        oracle_query,
        oracle_query_id,
        oracle_response,
        account_pubkey,
        signature,
        name,
        commitment,
        peer_pubkey,
        state,
        poi,
        state_trees,
        call_state_tree,
        bytearray,
    }
}

impl MockEncoder for api_encoder::KnownType {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            api_encoder::KnownType::KeyBlockHash => known_type::key_block_hash(),
            api_encoder::KnownType::MicroBlockHash => known_type::micro_block_hash(),
            api_encoder::KnownType::BlockPofHash => known_type::block_pof_hash(),
            api_encoder::KnownType::BlockTxHash => known_type::block_tx_hash(),
            api_encoder::KnownType::BlockStateHash => known_type::block_state_hash(),
            api_encoder::KnownType::Channel => known_type::channel(),
            api_encoder::KnownType::ContractBytearray => known_type::contract_bytearray(),
            api_encoder::KnownType::ContractPubkey => known_type::contract_pubkey(),
            api_encoder::KnownType::ContractStoreKey => known_type::contract_store_key(),
            api_encoder::KnownType::ContractStoreValue => known_type::contract_store_value(),
            api_encoder::KnownType::Transaction => known_type::transaction(),
            api_encoder::KnownType::TxHash => known_type::tx_hash(),
            api_encoder::KnownType::OraclePubkey => known_type::oracle_pubkey(),
            api_encoder::KnownType::OracleQuery => known_type::oracle_query(),
            api_encoder::KnownType::OracleQueryId => known_type::oracle_query_id(),
            api_encoder::KnownType::OracleResponse => known_type::oracle_response(),
            api_encoder::KnownType::AccountPubkey => known_type::account_pubkey(),
            api_encoder::KnownType::Signature => known_type::signature(),
            api_encoder::KnownType::Name => known_type::name(),
            api_encoder::KnownType::Commitment => known_type::commitment(),
            api_encoder::KnownType::PeerPubkey => known_type::peer_pubkey(),
            api_encoder::KnownType::State => known_type::state(),
            api_encoder::KnownType::Poi => known_type::poi(),
            api_encoder::KnownType::StateTrees => known_type::state_trees(),
            api_encoder::KnownType::CallStateTree => known_type::call_state_tree(),
            api_encoder::KnownType::Bytearray => known_type::bytearray(),
        }
        .encode(env)
    }
}

impl<'a> MockDecoder<'a> for api_encoder::KnownType {
    fn mock_decode(term: Term<'a>) -> NifResult<Self> {
        match term.atom_to_string()?.as_str() {
            "key_block_hash" => Ok(api_encoder::KnownType::KeyBlockHash),
            "micro_block_hash" => Ok(api_encoder::KnownType::MicroBlockHash),
            "block_pof_hash" => Ok(api_encoder::KnownType::BlockPofHash),
            "block_tx_hash" => Ok(api_encoder::KnownType::BlockTxHash),
            "block_state_hash" => Ok(api_encoder::KnownType::BlockStateHash),
            "channel" => Ok(api_encoder::KnownType::Channel),
            "contract_bytearray" => Ok(api_encoder::KnownType::ContractBytearray),
            "contract_pubkey" => Ok(api_encoder::KnownType::ContractPubkey),
            "contract_store_key" => Ok(api_encoder::KnownType::ContractStoreKey),
            "contract_store_value" => Ok(api_encoder::KnownType::ContractStoreValue),
            "transaction" => Ok(api_encoder::KnownType::Transaction),
            "tx_hash" => Ok(api_encoder::KnownType::TxHash),
            "oracle_pubkey" => Ok(api_encoder::KnownType::OraclePubkey),
            "oracle_query" => Ok(api_encoder::KnownType::OracleQuery),
            "oracle_query_id" => Ok(api_encoder::KnownType::OracleQueryId),
            "oracle_response" => Ok(api_encoder::KnownType::OracleResponse),
            "account_pubkey" => Ok(api_encoder::KnownType::AccountPubkey),
            "signature" => Ok(api_encoder::KnownType::Signature),
            "name" => Ok(api_encoder::KnownType::Name),
            "commitment" => Ok(api_encoder::KnownType::Commitment),
            "peer_pubkey" => Ok(api_encoder::KnownType::PeerPubkey),
            "state" => Ok(api_encoder::KnownType::State),
            "poi" => Ok(api_encoder::KnownType::Poi),
            "state_trees" => Ok(api_encoder::KnownType::StateTrees),
            "call_state_tree" => Ok(api_encoder::KnownType::CallStateTree),
            "bytearray" => Ok(api_encoder::KnownType::Bytearray),
            _ => Err(Error::BadArg),
        }
    }
}

mod decoding_err {
    rustler::atoms! {
        invalid_id_size,
        invalid_id_tag,
        invalid_id_pub,
        invalid_bool,
        invalid_int,
        invalid_binary,
        invalid_list,
        invalid_rlp,
        invalid_prefix,
        missing_prefix,
        incorrect_size,
        invalid_encoding,
        invalid_check,
        invalid_code,
    }
}

impl MockEncoder for aeser::error::DecodingErr {
    fn mock_encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            aeser::error::DecodingErr::InvalidIdSize =>         decoding_err::invalid_id_size(),
            aeser::error::DecodingErr::InvalidIdTag =>          decoding_err::invalid_id_tag(),
            aeser::error::DecodingErr::InvalidIdPub =>          decoding_err::invalid_id_pub(),
            aeser::error::DecodingErr::InvalidBool =>           decoding_err::invalid_bool(),
            aeser::error::DecodingErr::InvalidInt =>            decoding_err::invalid_int(),
            aeser::error::DecodingErr::InvalidBinary =>         decoding_err::invalid_binary(),
            aeser::error::DecodingErr::InvalidList =>           decoding_err::invalid_list(),
            aeser::error::DecodingErr::InvalidRlp =>            decoding_err::invalid_rlp(),
            aeser::error::DecodingErr::InvalidPrefix =>         decoding_err::invalid_prefix(),
            aeser::error::DecodingErr::MissingPrefix =>         decoding_err::missing_prefix(),
            aeser::error::DecodingErr::IncorrectSize =>         decoding_err::incorrect_size(),
            aeser::error::DecodingErr::InvalidEncoding =>       decoding_err::invalid_encoding(),
            aeser::error::DecodingErr::InvalidCheck =>          decoding_err::invalid_check(),
            aeser::error::DecodingErr::InvalidCode =>           decoding_err::invalid_code(),
        }.encode(env)
    }
}
