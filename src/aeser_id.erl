-module(aeser_id).

-export([ create/2
        , specialize/1
        , specialize/2
        , specialize_type/1
        , is_id/1
        ]).

%% For aec_serialization
-export([ encode/1
        , decode/1
        ]).

-record(id, { tag
            , val
            }).

-type tag() :: 'account' | 'oracle' | 'name'
             | 'commitment' | 'contract' | 'channel'.
-type val() :: <<_:256>>.
-opaque(id() :: #id{}).

-define(IS_TAG(___TAG___), ___TAG___ =:= account;
                           ___TAG___ =:= oracle;
                           ___TAG___ =:= name;
                           ___TAG___ =:= commitment;
                           ___TAG___ =:= contract;
                           ___TAG___ =:= channel
       ).
-define(IS_VAL(___VAL___), byte_size(___VAL___) =:= 32).

-export_type([ id/0
             , tag/0
             , val/0
             ]).

-spec create(tag(), val()) -> id().
create(Tag, Val) when ?IS_TAG(Tag), ?IS_VAL(Val) ->
    #id{ tag = Tag
       , val = Val};
create(Tag, Val) when ?IS_VAL(Val) ->
    error({illegal_tag, Tag});
create(Tag, Val) when ?IS_TAG(Tag)->
    error({illegal_val, Val});
create(Tag, Val) ->
    error({illegal_tag_and_val, Tag, Val}).


-spec specialize(id()) -> {tag(), val()}.
specialize(#id{tag = Tag, val = Val}) ->
    {Tag, Val}.

-spec specialize(id(), tag()) -> val().
specialize(#id{tag = Tag, val = Val}, Tag) when ?IS_TAG(Tag), ?IS_VAL(Val) ->
    Val.

-spec specialize_type(id()) -> tag().
specialize_type(#id{tag = Tag}) when ?IS_TAG(Tag) ->
    Tag.

-spec is_id(term()) -> boolean().
is_id(#id{}) -> true;
is_id(_) -> false.


-spec encode(id()) -> binary().
encode(Id = #id{}) ->
    aeser_nif:id_encode(Id).

-spec decode(binary()) -> id().
decode(Bin)  ->
    aeser_nif:id_decode(Bin).
