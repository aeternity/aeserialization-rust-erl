-module(aesr_rlp_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(UNTAGGED_SIZE_LIMIT , 55).
-define(UNTAGGED_LIMIT      , 127).
-define(BYTE_ARRAY_OFFSET   , 128).
-define(LIST_OFFSET         , 192).

-define(TEST_MODULE, aesr_rlp).
-define(REF_MODULE, aeser_rlp).

rlp() ->
    ?SIZED(Depth, rlp(Depth)).

rlp(Depth) ->
    rlp(Depth, 2).

rlp(0, _) ->
    ?LET({I, B}, {int(), binary()}, <<(abs(I) rem 255), B/binary>>);
rlp(Depth, Span) ->
    frequency(
      [ {5, rlp(0, Span)}
      , {3, ?LAZY(?LETSHRINK(L, list(Span, rlp(Depth div 2, Span * 2)), L))}
      ]
     ).

prop_encode() ->
    ?FORALL(
       B, rlp(),
       eqc:equals(?TEST_MODULE:encode(B), ?REF_MODULE:encode(B))
      ).

-define(catch_decode(E),
        try E of
            X -> X
        catch _:Err ->
                if is_tuple(Err) ->
                        element(1, Err);
                   true -> Err
                end
        end
        ).

prop_decode() ->
    ?FORALL(
       B, rlp(0),
       eqc:equals(?catch_decode(?TEST_MODULE:decode(B)),
                  ?catch_decode(?REF_MODULE:decode(B))
                 )
      ).


prop_decode_one() ->
    ?FORALL(
       B, rlp(0),
       eqc:equals(?catch_decode(?TEST_MODULE:decode_one(B)),
                  ?catch_decode(?REF_MODULE:decode_one(B))
                 )
      ).
