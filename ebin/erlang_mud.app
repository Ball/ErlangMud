{application, erlang_mud,
  [{description, "A simple erlang mud"},
   {vsn, "0.1.0"},
   {modules, [
                erlang_mud_app,
                erlang_mud_sup,
                player,
                player_proxy,
                registry,
                room
             ]},
   {registered, [registry,erlang_mud_sup]},
   {applications, [kernel, stdlib]},
   {mod, {erlang_mud_app, []}}
  ]
}.
