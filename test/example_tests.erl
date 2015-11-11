-module(example_tests).
-author("Tomasz Lichon").

-include_lib("eunit/include/eunit.hrl").

-export([assert_test/0]).

assert_test() ->
    ?assert(true).
