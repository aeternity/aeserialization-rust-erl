-module(aeser_rlp).

-export([encode/1,
         decode/1,
         decode_one/1
        ]).

-include("cargo.hrl").
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

%%%===================================================================
%%% NIF
%%%===================================================================

encode(_Data) ->
    ?NOT_LOADED.


decode(_Data) ->
    ?NOT_LOADED.


decode_one(_Data) ->
    ?NOT_LOADED.


init() ->
    ?load_nif_from_crate(aeser_erl, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
