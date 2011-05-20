-include_lib("eunit/include/eunit.hrl").
-define(It(Text, Setup, Cleanup, Func),
        {"It " ++ Text, setup, Setup, Cleanup, ?_test(Func)}).
-define(Describe(Text,Tests),{"Describe "++Text, Tests}).

% TODO : propegate setup and cleanup to the child tests
-define(Describe(Text,Setup,Cleanup,Tests),{Text,Tests}).

-define(_fail(),?_assert(false)).
