set BASE_DIR=c:/app/deployment/tools_platform
erl -config %BASE_DIR%/local_win_remote -pa %BASE_DIR%/remote/ebin -s remote
