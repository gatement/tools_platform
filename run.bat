set BASE_DIR=C:/app/tools_platform
set YAWS_EBIN=C:/dev/Yaws/ebin
set CONFIG_FILE=local
erl -config %BASE_DIR%/%CONFIG_FILE% -pa %BASE_DIR%/deps/uuid/ebin %BASE_DIR%/apps/platform_core/ebin %BASE_DIR%/apps/cleaner/ebin %BASE_DIR%/apps/db_backup/ebin %BASE_DIR%/apps/interface_http/ebin %BASE_DIR%/apps/iptracker/ebin %YAWS_EBIN% -s platform_core
