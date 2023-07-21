-module(aesr_rlp_eqc_SUITE).

-compile(export_all).

-include_lib("eqc/include/eqc_ct.hrl").

all() ->
    [check_prop_encode,
     check_prop_decode,
     check_prop_decode_one].

check_prop_encode(_) ->
    ?quickcheck((aesr_rlp_eqc:prop_encode())).

check_prop_decode(_) ->
    ?quickcheck((aesr_rlp_eqc:prop_decode())).

check_prop_decode_one(_) ->
    ?quickcheck((aesr_rlp_eqc:prop_decode_one())).

