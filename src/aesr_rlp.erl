-module(aesr_rlp).

-export([encode/1,
         decode/1,
         decode_one/1
        ]).

-export_type([ encodable/0
             , encoded/0
             ]).

-type encodable() :: [encodable()] | binary().
-type encoded()   :: <<_:8, _:_*8>>.


-spec encode(encodable()) -> encoded().
encode(X) ->
    aesr_nif:rlp_encode(X).


-spec decode(encoded()) -> encodable().
decode(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    aesr_nif:rlp_decode(Bin).

decode_one(Bin) ->
    aesr_nif:rlp_decode_one(Bin).
