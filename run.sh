erl -pa ../tools_platform/ebin ./apps/monitor/ebin ./apps/session_cleaner/ebin  /usr/local/lib/yaws/ebin -config local -mnesia dir '"/app/mnesia/tools_platform"' -s tools_platform
