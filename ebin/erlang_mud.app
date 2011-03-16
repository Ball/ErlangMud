{application, erlang_mud,
  [{description, "A simple erlang mud"},
   {vsn, "0.1.0"},
   {modules, [
                em_app,
                em_sup,
             ]},
   {registered, [em_sup]},
   {applications, [kernel, stdlib]},
   {mod, {em_app, []}}
  ]
}.
