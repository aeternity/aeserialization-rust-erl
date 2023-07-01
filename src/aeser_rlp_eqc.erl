
-module(aeser_rlp_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(UNTAGGED_SIZE_LIMIT , 55).
-define(UNTAGGED_LIMIT      , 127).
-define(BYTE_ARRAY_OFFSET   , 128).
-define(LIST_OFFSET         , 192).

-define(TEST_MODULE, aeser_rlp).

prop_rlp() ->
    ?FORALL(B, ?SUCHTHAT(Bb, binary(), Bb =/= <<>>),
            begin
                E = ?TEST_MODULE:encode(B),
                D = ?TEST_MODULE:decode(E),
                B == D
            end
      ).
