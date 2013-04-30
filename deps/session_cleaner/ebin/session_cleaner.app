{application, session_cleaner,
 [{description, "session cleaner"},
  {vsn, "0.1.0"},
  {modules, [session_cleaner, sc_app, sc_sup, sc_server]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib]},
  {env, [
    {run_interval, 60}, % in seconds
    {user_session_timeout, 57600} % in seconds, = 16hr
  ]},
  {mod, {sc_app, []}}
 ]}.
