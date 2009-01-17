{application, gamewad,
 [{description, "gamewad"},
  {vsn, "0.01"},
  {modules, [
    gamewad,
    gamewad_app,
    gamewad_sup,
    gamewad_web,
    gamewad_deps
  ]},
  {registered, []},
  {mod, {gamewad_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
