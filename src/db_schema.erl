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

	%% == lowing function is not for initialization ====
	ok.


%% ===================================================================
%% base update
%% ===================================================================

create_base_schema() ->
	mnesia:start(),

	% table schema
	mnesia:change_table_copy_type(schema, node(), disc_copies),

	% table gbl_setting
	mnesia:create_table(gbl_setting, [{attributes, record_info(fields, gbl_setting)}, {disc_copies, [node()]}]),

	% table usr_user
	mnesia:create_table(usr_user, [{attributes, record_info(fields, usr_user)}, {disc_copies, [node()]}]),

	% table usr_preference
	mnesia:create_table(usr_preference, [{attributes, record_info(fields, usr_preference)}, {disc_copies, [node()]}]),
	
	% table usr_session
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
	model_setting:set(?GBL_SETTING_EMAIL, "gatement@gmail.com"),
	model_setting:set(?GBL_SETTING_EMAIL_PASSWORD, "_fake_email_password_"),
	model_setting:set(?GBL_SETTING_SITE_URL, "https://tools.johnson.uicp.net"),

	ok.
	

%% ===================================================================
%% migration update
%% ===================================================================


%% ===================================================================
%% Local Functions
%% ===================================================================
