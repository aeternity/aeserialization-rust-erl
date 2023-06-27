use rustler::*;

pub struct ErlBin {pub bytes: Vec<u8>}

impl ErlBin {
    pub fn new(bytes: &[u8]) -> ErlBin {
        ErlBin{bytes: bytes.to_vec()}
    }
}

impl Encoder for ErlBin {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let mut bin = types::binary::NewBinary::new(env, self.bytes.len());
        let data = bin.as_mut_slice();
        data.copy_from_slice(&self.bytes);
        bin.encode(env)
    }
}

impl<'a> Decoder<'a> for ErlBin {
    fn decode(term: Term<'a>) -> NifResult<ErlBin> {
        let bin = term.to_binary();
        let data = bin.as_slice();
        Ok(ErlBin{bytes: data.to_vec()})
    }
}
