set BASE_DIR=c:/app/deployment/tools_platform
erl -config %BASE_DIR%/local_win -pa %BASE_DIR%/platform_core/deps/uuid/ebin %BASE_DIR%/platform_core/ebin %BASE_DIR%/cleaner/ebin %BASE_DIR%/db_backup/ebin %BASE_DIR%/monitor/ebin %BASE_DIR%/mqtt_broker/ebin %BASE_DIR%/mqtt_client/ebin %BASE_DIR%/interface_http/ebin %BASE_DIR%/iptracker/ebin %BASE_DIR%/remote/ebin %YAWS_EBIN% -s remote
