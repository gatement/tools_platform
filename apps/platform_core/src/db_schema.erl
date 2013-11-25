-module(db_schema).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([up/0, setup/0]).


%% ===================================================================
%% API Functions
%% ===================================================================

up() ->
    delete_mqtt_and_device_tables(),

	ok.


setup() ->
	create_base_schema(),	
	init_base_data(),

	create_word_schema(),
	create_note_schema(),
	create_gallery_schema(),

	ok.


%% ===================================================================
%% Base Update
%% ===================================================================

create_base_schema() ->
	mnesia:start(),
	mnesia:change_table_copy_type(schema, node(), disc_copies),

	mnesia:create_table(gbl_setting, [{attributes, record_info(fields, gbl_setting)}, {disc_copies, [node()]}]),
	mnesia:create_table(usr_user, [{attributes, record_info(fields, usr_user)}, {disc_copies, [node()]}]),
	mnesia:create_table(usr_preference, [{attributes, record_info(fields, usr_preference)}, {disc_copies, [node()]}]),	
	mnesia:create_table(usr_session, [{attributes, record_info(fields, usr_session)}, {disc_copies, [node()]}]),
	
	ok.


init_base_data() ->
	%% user admin
	model_usr_user:create(#usr_user{id = "admin", 
		                        password = "admin", 
		                        name = "Johnson Lau", 
		                        email = "gatement@gmail.com",
		                        enabled = true,
		                        admin = true}),

	%% default gbl_setting
	model_gbl_setting:set(?GBL_SETTING_EMAIL, "gatement@gmail.com"),
	model_gbl_setting:set(?GBL_SETTING_EMAIL_PASSWORD, "_fake_email_password_"),
	model_gbl_setting:set(?GBL_SETTING_SITE_URL, "https://tools.johnson.uicp.net"),

	ok.
	

create_word_schema() ->
	mnesia:create_table(wrd_word, [{attributes, record_info(fields, wrd_word)}, {disc_copies, [node()]}]),

	ok.
	

create_note_schema() ->
	mnesia:create_table(nte_note, [{attributes, record_info(fields, nte_note)}, {disc_copies, [node()]}]),
	mnesia:create_table(nte_category, [{attributes, record_info(fields, nte_category)}, {disc_copies, [node()]}]),
	mnesia:create_table(nte_share, [{attributes, record_info(fields, nte_share)}, {disc_copies, [node()]}]),
	mnesia:create_table(nte_history, [{attributes, record_info(fields, nte_history)}, {disc_copies, [node()]}]),

	ok.


create_gallery_schema() ->
	mnesia:create_table(gly_item, [{attributes, record_info(fields, gly_item)}, {disc_copies, [node()]}]),
	mnesia:create_table(gly_share, [{attributes, record_info(fields, gly_share)}, {disc_copies, [node()]}]),

	ok.


%% ===================================================================
%% Migration Update
%% ===================================================================

delete_mqtt_and_device_tables() ->
    mnesia:delete_table(dev_device),
    mnesia:delete_table(dev_device_user),
    mnesia:delete_table(dev_status),
    mnesia:delete_table(dev_data),
    mnesia:delete_table(mqtt_session),
    mnesia:delete_table(mqtt_subscription),
    mnesia:delete_table(mqtt_pub_permission),
    mnesia:delete_table(mqtt_pub_queue),
    mnesia:delete_table(mqtt_message_id),

    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
