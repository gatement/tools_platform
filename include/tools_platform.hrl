%% definition ==================================
%% running definitions ----------
-define(USR_SESSION_COOKIE_ID, "usr_sid").

%% global_setting keys ----------
-define(GBL_SETTING_EMAIL, "email").
-define(GBL_SETTING_EMAIL_PASSWORD, "email_password").
-define(GBL_SETTING_SITE_URL, "site_url").

%% user preference keys ----------
-define(USR_PREFERENCE_NOTE_ENABLED, "note").
-define(USR_PREFERENCE_WORD_ENABLED, "word").
-define(USR_PREFERENCE_SPORT_ENABLED, "sport").
-define(USR_PREFERENCE_GALLERY_ENABLED, "gallery").

%% runing records ==============================
-record(arg_state, {session_id, user_id, user_name, other}).
-record(user_search_result, {id, name, email, enabled, admin, last_login, note, word, sport, gallery}).
-record(user_autocomplete_item, {label, value}).

%% database tables =============================
%% global ------------------
-record(gbl_setting, {key, value}).

%% user --------------------
-record(usr_user, {id, password, name, email, enabled, admin, last_login}).
-record(usr_preference, {id, user_id, key, value}).
-record(usr_session, {id, user_id, user_name, last_active}).
