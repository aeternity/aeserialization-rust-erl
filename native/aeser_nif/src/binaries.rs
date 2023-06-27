use rustler::*;

pub struct ErlBin {pub bytes: Vec<u8>}

impl ErlBin {
    pub fn new(bytes: &[u8]) -> ErlBin {
        ErlBin{bytes: bytes.to_vec()}
    }

    pub fn to_binary<'a>(&self, env: Env<'a>) -> types::binary::Binary<'a> {
        let mut bin = types::binary::NewBinary::new(env, self.bytes.len());
        let data = bin.as_mut_slice();
        data.copy_from_slice(&self.bytes);

        Binary::from(bin)
    }
}

impl Encoder for ErlBin {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.to_binary(env).to_term(env)
    }
}

impl<'a> Decoder<'a> for ErlBin {
    fn decode(term: Term<'a>) -> NifResult<ErlBin> {
        if !term.is_binary() {
            Err(Error::BadArg)?;
        }

        let bin = Binary::from_term(term)?;
        let data = bin.as_slice();
        Ok(ErlBin{bytes: data.to_vec()})
    }
}
