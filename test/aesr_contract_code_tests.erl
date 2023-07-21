-module(aesr_contract_code_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DUMMY_CODE_MAP,
        #{ byte_code => <<"DUMMY_CODE">>
         , type_info => []
         , compiler_version => <<"3.1.4">>
         , source_hash => <<"totally a hash">>
         %% , contract_source => "contract Foo = ..."
         , payable => true} ).

vsn_3_test() ->
    aesr_contract_code:deserialize(
        aesr_contract_code:serialize(?DUMMY_CODE_MAP)).
