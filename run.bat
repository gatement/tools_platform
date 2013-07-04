set BASE_DIR=c:/app/tools_platform
set YAWS_EBIN=C:/dev/Yaws/ebin
erl -config %BASE_DIR%/local_win -pa %BASE_DIR%/deps/uuid/ebin %BASE_DIR%/apps/platform_core/ebin %BASE_DIR%/apps/cleaner/ebin %BASE_DIR%/apps/db_backup/ebin %BASE_DIR%/apps/monitor/ebin %BASE_DIR%/apps/mqtt_broker/ebin %BASE_DIR%/apps/mqtt_client/ebin %BASE_DIR%/apps/interface_http/ebin %BASE_DIR%/apps/iptracker/ebin %YAWS_EBIN% -s platform_core
