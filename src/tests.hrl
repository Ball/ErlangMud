-include_lib("eunit/include/eunit.hrl").
-define(It(Text,Func), {"It " ++ Text, Func}).
-define(It(Text,Setup,Cleanup,Func),
        {"It " ++ Text, setup,Setup,Cleanup,Func}).
-define(Describe(Text,Tests),{Text, Tests}).

