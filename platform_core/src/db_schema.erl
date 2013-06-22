-module(db_schema).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).


%% ===================================================================
%% API functions
%% ===================================================================

up() ->
	%% == initialization functions =====================
	create_base_schema(),
	init_base_data(),
	create_word_schema(),
	create_note_schema(),
	create_gallery_schema(),
	create_mqtt_schema(),
	create_device_schema(),

	%% == lowing function is not for initialization ====
	add_socket_to_usr_session(),
	add_mqtt_main_client_subscription(),

	ok.


%% ===================================================================
%% base update
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
	% user admin
	model_usr_user:create(#usr_user{id = "admin", 
		                        password = "admin", 
		                        name = "Johnson Lau", 
		                        email = "gatement@gmail.com",
		                        enabled = true,
		                        admin = true}),

	% default gbl_setting
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


create_mqtt_schema() ->
	mnesia:create_table(mqtt_session, [{attributes, record_info(fields, mqtt_session)}, {ram_copies, [node()]}]),
	mnesia:create_table(mqtt_subscription, [{attributes, record_info(fields, mqtt_subscription)}, {disc_copies, [node()]}]),

	ok.


create_device_schema() ->
	mnesia:create_table(dev_device, [{attributes, record_info(fields, dev_device)}, {disc_copies, [node()]}]),
	mnesia:create_table(dev_status, [{attributes, record_info(fields, dev_status)}, {ram_copies, [node()]}]),
	mnesia:create_table(dev_data, [{attributes, record_info(fields, dev_data)}, {disc_copies, [node()]}]),

	ok.


%% ===================================================================
%% migration update
%% ===================================================================

add_socket_to_usr_session() ->
	%% add column socket_pid, socket_territory to table usr_session
	Fun = fun({usr_session, Id, UserId, UserName, LastActive}) ->
		#usr_session{id = Id, 
			user_id = UserId, 
			user_name = UserName, 
			last_active = LastActive}
	end,

	mnesia:transform_table(usr_session, Fun, record_info(fields, usr_session)).


add_mqtt_main_client_subscription() ->
	model_mqtt_subscription:create(#mqtt_subscription{
		id = uuid:to_string(uuid:uuid1()), 
		name = "MQTT main client",
		client_id = "000000000001", 
		topic = "#",
		qos = 0
	}).


%% ===================================================================
%% Local Functions
%% ===================================================================
