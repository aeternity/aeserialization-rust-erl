-module(aeser_contract_code).

-export([ deserialize/1
        , serialize/1
        ]).

-spec serialize(map()) -> binary().
serialize(CodeMap) ->
    aeser_nif:contract_code_serialize(CodeMap).

-spec deserialize(binary()) -> map().
deserialize(Binary) ->
    aeser_nif:contract_code_deserialize(Binary).
