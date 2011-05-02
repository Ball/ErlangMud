-include_lib("eunit/include/eunit.hrl").
-define(It(Text,Func), {"It " ++ Text, Func}).
-define(It(Text,Setup,Cleanup,Func),
        {"It " ++ Text, setup,Setup,Cleanup,Func}).
-define(Describe(Text,Tests),{Text, Tests}).
% TODO : propegate setup and cleanup to the child tests
-define(Describe(Text,Setup,Cleanup,Tests),{Text,Tests}).

-define(_fail(),?_assert(false)).
