%% definition ===============================================
%% running definitions ----------
-define(USR_SESSION_COOKIE_ID, "usr_sid").

%% global_setting keys -------------
-define(GBL_SETTING_EMAIL, "email").
-define(GBL_SETTING_EMAIL_PASSWORD, "email_password").
-define(GBL_SETTING_SITE_URL, "site_url").

%% user preference keys ------------
-define(USR_PREFERENCE_NOTE_ENABLED, "note").
-define(USR_PREFERENCE_WORD_ENABLED, "word").
-define(USR_PREFERENCE_SPORT_ENABLED, "sport").
-define(USR_PREFERENCE_GALLERY_ENABLED, "gallery").
-define(USR_PREFERENCE_MONITOR_ENABLED, "monitor").

%% default note parameters ----------
-define(NOTE_DEFAULT_LEFT, 5).
-define(NOTE_DEFAULT_TOP, 39).
-define(NOTE_DEFAULT_WIDTH, 220).
-define(NOTE_DEFAULT_HEIGHT, 170).
-define(NOTE_DEFAULT_COLOR, 6).


%% runing records ===========================================
-record(gly_item_upload, {item_id, item_name, album_item_id, mime_type, user_id, path, fd, file_full_name, last}).
-record(arg_state, {session_id, user_id, user_name}).
-record(user_search_result, {id, name, email, enabled, admin, last_login, note, word, sport, gallery, monitor}).
-record(user_autocomplete_item, {label, value}).
-record(note_category, {id, name, permission, is_default, is_trash, display_order}).
-record(note_share, {id, category_id, user_id, user_name, share_type}).
-record(gallery_item, {id, name, thumbnail_url, original_url, type, height}).


%% database tables ===========================================
%% global ------------------
-record(gbl_setting, {key, value}).

%% user --------------------
-record(usr_user, {id, password, name, email, enabled, admin, last_login}).
-record(usr_preference, {id, user_id, key, value}).
-record(usr_session, {id, user_id, user_name, last_active}).

%% word --------------------
-record(wrd_word, {id, user_id, word, pronunciation, translation, display_order, last_updated}).

%% note --------------------
-record(nte_note, {id, category_id, note, left, top, width, height, color, z_index, last_updated}).
-record(nte_category, {id, user_id, name, is_default, display_order, attributes}).
-record(nte_share, {id, category_id, user_id, share_type}).
-record(nte_history, {id, note_id, note, datetime}).

%% gallery --------------------
-record(gly_item, {id, user_id, parent_id, name, path, type, mime_type, display_order, created}).
-record(gly_share, {id, item_id, user_id, share_type}).
