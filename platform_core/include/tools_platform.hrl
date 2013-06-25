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
-define(USR_PREFERENCE_GALLERY_ENABLED, "gallery").
-define(USR_PREFERENCE_MONITOR_ENABLED, "monitor").
-define(USR_PREFERENCE_DEVICE_ENABLED, "device").

%% default note parameters ----------
-define(NOTE_DEFAULT_LEFT, 5).
-define(NOTE_DEFAULT_TOP, 39).
-define(NOTE_DEFAULT_WIDTH, 220).
-define(NOTE_DEFAULT_HEIGHT, 170).
-define(NOTE_DEFAULT_COLOR, 6).


%% database tables ===========================================
%% global ------------------
-record(gbl_setting, {key, value}).

%% user --------------------
-record(usr_user, {id, password, name, email, enabled, admin, last_login}).
-record(usr_preference, {id, user_id, key, value}).
-record(usr_session, {id, user_id, user_name, last_active, socket_pid, socket_territory}).

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

%% device --------------------
-record(dev_device, {device_id, name, user_id, type, created}).
-record(dev_device_user, {id, device_id, user_id}).
-record(dev_status, {id, device_id, key, value, updated}).
-record(dev_data, {id, device_id, key, value, datetime}).

%% mqtt --------------------
-record(mqtt_session, {client_id, pid, created}).
-record(mqtt_subscription, {id, client_id, topic, qos, desc}).
-record(mqtt_pub_permission, {id, topic, client_id, user_id, desc}).

%% runing records ===========================================
-record(gly_item_upload, {item_id, item_name, album_item_id, mime_type, user_id, path, fd, file_full_name, last}).
-record(arg_state, {session_id, user_id, user_name}).
-record(user_search_result, {id, name, email, enabled, admin, last_login, note, word, gallery, monitor, device}).
-record(user_autocomplete_item, {label, value}).
-record(note_category, {id, name, permission, is_default, is_trash, display_order}).
-record(note_share, {id, category_id, user_id, user_name, share_type}).
-record(gallery_item, {id, name, thumbnail_url, original_url, type, height}).
-record(gallery_item_info, {id, parent_id, ancestor_path, permission}).
-record(gallery_share, {id, item_id, user_id, user_name, share_type}).
