{application, monitor,
 [{description, "system monitor"},
  {vsn, "0.1.0"},
  {modules, [monitor, mon_app, mon_sup, mon_server]},
  {registered, [mon_sup]},
  {applications, [kernel, stdlib]},
  {env, [
    {run_interval, 60}, % in seconds
    {log_file_name, "./log.csv"}
  ]},
  {mod, {mon_app, []}}
 ]}.
