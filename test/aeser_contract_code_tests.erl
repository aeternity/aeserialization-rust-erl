-module(aeser_contract_code_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DUMMY_TYPE_INFO,
        #{ type_hash => <<"type">>
         , name => <<"name">>
         , payable => false
         , arg_type => <<"arg">>
         , out_type => <<"out">>
         }
       ).

-define(DUMMY_CODE_MAP,
        #{ byte_code => <<"DUMMY CODE">>
         , type_info => [?DUMMY_TYPE_INFO]
         , compiler_version => <<"3.1.4">>
         , source_hash => <<"totally a hash">>
         %% , contract_source => "contract Foo = ..."
         , payable => true} ).

vsn_3_test() ->
    aeser_contract_code:deserialize(
        aeser_contract_code:serialize(?DUMMY_CODE_MAP)).
