-module(aeser_nif).

-export([rlp_encode/1,
         rlp_decode/1,
         rlp_decode_one/1,
         id_encode/1,
         id_decode/1
        ]).


-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

init() ->
    ?load_nif_from_crate(aeser_nif, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% NIF
%%%===================================================================


id_encode(_Id) ->
    ?NOT_LOADED.

id_decode(_Bin) ->
   ?NOT_LOADED.

rlp_encode(_Data) ->
    ?NOT_LOADED.

rlp_decode(_Data) ->
    ?NOT_LOADED.

rlp_decode_one(_Data) ->
    ?NOT_LOADED.
