-module(rest_tool_note).
-include("yaws_api.hrl").
-include("../../core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	NoteEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_NOTE_ENABLED),

	case NoteEnabled of
		true ->
			case Arg#arg.pathinfo of
				undefined -> 
					note_html();
				_ -> 
					out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
			end;

		false -> 
			{status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

%% ==== note =========================================================

out(Arg, ["note", "add"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),
	ZIndex = erlang:list_to_integer(proplists:get_value("z_index", Vals)),

    Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
		        none -> [{"success", false}, {"data", "You don't have permission to add note to this category."}];
		        r -> [{"success", false}, {"data", "You don't have permission to add note to this category."}];
		        _ -> 
		            Note = #nte_note{category_id = CategoryId, 
		                             note = "", 
		                             left = ?NOTE_DEFAULT_LEFT, 
		                             top = ?NOTE_DEFAULT_TOP, 
		                             width = ?NOTE_DEFAULT_WIDTH, 
		                             height = ?NOTE_DEFAULT_HEIGHT, 
		                             color = ?NOTE_DEFAULT_COLOR, 
		                             z_index = ZIndex},
		            case model_nte_note:create(Note, UserId) of
		                error -> 
		                    [{"success", false}, {"data", "Create note error."}];
		                Note2 ->
		                    [{"success", true}, {"data", {struct, [{"permission", "rw"} | tools:record_to_list(Note2, record_info(fields, nte_note))]}}]
		            end
		    end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "load"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	CategoryId = proplists:get_value("category_id", Vals),

    Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
                none -> [{"success", false}, {"data", "You don't have permission to load note from current category."}];
                Permission -> 
                    case model_nte_note:get(NoteId, CategoryId) of
                        error -> [{"success", false}, {"data", "Failed to load note."}];
                        Note -> [{"success", true}, {"data", {struct, [{"permission", erlang:atom_to_list(Permission)} | tools:record_to_list(Note, record_info(fields, nte_note))]}}]
                    end
            end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "list"], UserId) -> 
	Vals = yaws_api:parse_query(Arg),
	CategoryId = proplists:get_value("category_id", Vals),

    Result = 
    case CategoryId of
    	"0" -> 
    		Notes = model_nte_note:list_mine(UserId),	
            NoteList = [{struct, [{"permission", "r"} | tools:record_to_list(Note, record_info(fields, nte_note))]} || Note <- Notes],
            [{"success", true}, {"data", {array, NoteList}}];
    	_ ->
    		case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
		        none -> [{"success", false}, {"data", "You don't have permission to load notes from current category."}];
		        Permission -> 
		            Notes = model_nte_note:list(CategoryId),
		            NoteList = [{struct, [{"permission", erlang:atom_to_list(Permission)} | tools:record_to_list(Note, record_info(fields, nte_note))]} || Note <- Notes],
		            [{"success", true}, {"data", {array, NoteList}}]
		    end
	end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "category"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	NewCategoryId = proplists:get_value("new_category_id", Vals),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
		        owner ->
		            case model_nte_share:get_permission_by_category_id(UserId, NewCategoryId) of
		                none -> [{"success", false}, {"data", "You don't have permission to update this note."}];
		                r -> [{"success", false}, {"data", "You don't have permission to update this note."}];
		                _ ->
		                    case model_nte_note:update_note_category(NoteId, NewCategoryId) of
		                        error ->
		                            [{"success", false}, {"data", "Failed to update note content."}];
		                        _ ->
		                            [{"success", true}, {"data", "ok."}]
		                    end
		            end;

		        _ -> [{"success", false}, {"data", "You don't have permission to change this note's category."}]
		    end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "size"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	Width = erlang:list_to_integer(proplists:get_value("width", Vals)),
	Height = erlang:list_to_integer(proplists:get_value("height", Vals)),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "You don't have permission to update this note size."}];
                r -> [{"success", false}, {"data", "You don't have permission to update this note size."}];
                _ ->
                    case model_nte_note:update_note_size(NoteId, Width, Height) of
                        ok -> 
                            [{"success", true}, {"data", "ok."}];
                        error ->
                            [{"success", false}, {"data", "Failed to update note size."}]
                    end
            end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "position"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	Left = erlang:list_to_integer(proplists:get_value("left", Vals)),
	Top = erlang:list_to_integer(proplists:get_value("top", Vals)),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "You don't have permission to update this note position."}];
                r -> [{"success", false}, {"data", "You don't have permission to update this note position."}];
                _ ->
                    case model_nte_note:update_note_position(NoteId, Left, Top) of
                        ok -> 
                            [{"success", true}, {"data", "ok."}];
                        error ->
                            [{"success", false}, {"data", "Failed to update note position."}]
                    end
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "z_index"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	ZIndex = erlang:list_to_integer(proplists:get_value("z_index", Vals)),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "You don't have permission to update this note z-inex."}];
                r -> [{"success", false}, {"data", "You don't have permission to update this note z-inex."}];
                _ ->
                    case model_nte_note:update_note_z_index(NoteId, ZIndex) of
                        ok -> 
                            [{"success", true}, {"data", "ok."}];
                        error ->
                            [{"success", false}, {"data", "Failed to update note z_index."}]
                    end
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "color"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	Color = erlang:list_to_integer(proplists:get_value("color", Vals)),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
	                none -> [{"success", false}, {"data", "You don't have permission to update this note's color."}];
	                r -> [{"success", false}, {"data", "You don't have permission to update this note's color."}];
	                _ ->
	                    case model_nte_note:update_note_color(NoteId, Color) of
	                        ok ->
	                            [{"success", true}, {"data", "ok."}];
	                        error ->
	                            [{"success", false}, {"data", "Failed to update note color."}]
	                    end
	            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "update", "content"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),
	Content = proplists:get_value("content", Vals),

    Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "You don't have permission to update this note."}];
                r -> [{"success", false}, {"data", "You don't have permission to update this note."}];
                _ ->
                    case model_nte_note:update_note_content(NoteId, Content, UserId) of
                        error ->
                            [{"success", false}, {"data", "Failed to update note content."}];
                        _ ->
                            [{"success", true}, {"data", "ok."}]
                    end
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "delete"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),

	Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "You don't have permission to delete this note."}];
                r -> [{"success", false}, {"data", "You don't have permission to delete this note."}];
                _ ->
                	case model_nte_note:get(NoteId) of
                		#nte_note{category_id = CategoryId} ->
                			#nte_category{attributes = Attributes} = model_nte_category:get(CategoryId),
                			case lists:member(trash, Attributes) of
                				true ->
                					%% delete it
  									case model_nte_note:delete(NoteId) of
				                        error ->
				                            [{"success", false}, {"data", "Failed to delete note."}];
				                        ok ->
				                        	model_nte_history:delete_by_noteId(NoteId),
				                            [{"success", true}, {"data", "ok."}]
				                    end;
				                _ ->
				                	%% move to trash category
				                	model_nte_note:move_note_to_trash(NoteId, UserId),
				                	[{"success", true}, {"data", "ok."}]
				            end;
                		_ ->
                			[{"success", true}, {"data", "ok."}]
                	end				                  
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["note", "arrange", "notes"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),

    Fun = fun(Index) ->
		NoteIdKey = lists:flatten(io_lib:format("notes[~p][id]", [Index])),
		LeftKey = lists:flatten(io_lib:format("notes[~p][left]", [Index])),
		TopKey = lists:flatten(io_lib:format("notes[~p][top]", [Index])),
		WidthKey = lists:flatten(io_lib:format("notes[~p][width]", [Index])),
		HeightKey = lists:flatten(io_lib:format("notes[~p][height]", [Index])),

        NoteId = proplists:get_value(NoteIdKey, Vals),
        Left = erlang:list_to_integer(proplists:get_value(LeftKey, Vals)),
        Top = erlang:list_to_integer(proplists:get_value(TopKey, Vals)),
        Width = erlang:list_to_integer(proplists:get_value(WidthKey, Vals)),
        Height = erlang:list_to_integer(proplists:get_value(HeightKey, Vals)),

        case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
            none -> no_permission;
            r -> no_permission;
            _ -> 
                if
                    Left =:= undefined -> do_nothing;
                    Left =:= -1 -> do_nothing;
                    true -> model_nte_note:update_note_position(NoteId, Left, Top)
                end,

                if
                    Width =:= undefined -> do_nothing;
                    Width =:= -1 -> do_nothing;
                    true -> model_nte_note:update_note_size(NoteId, Width, Height)
                end
        end
    end,

	Length = erlang:round(erlang:length(Vals)/6),
	LengthList = lists:seq(0, Length - 1),
	lists:foreach(Fun, LengthList),

	Result = [{"success", true}, {"data", "ok."}],

    {content, "application/json", json2:encode({struct, Result})};


%% ==== category =====================================================

out(Arg, ["category", "add"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryName = proplists:get_value("category_name", Vals),
	IsDefault = erlang:list_to_atom(proplists:get_value("is_default", Vals)),

	Result = case model_nte_category:exist(UserId, CategoryName) of
	                true -> [{"success", false}, {"data", "existing."}];
	                false ->
	                    %clear is_default value firstly if need
	                    case IsDefault of
	                        true -> model_nte_category:clear_is_default(UserId);
	                        _ -> ok
	                    end,

	                    NoteCategory = #nte_category{user_id = UserId, name = CategoryName, is_default=IsDefault},
	                    case model_nte_category:create(NoteCategory) of
	                        error -> 
	                            [{"success", false}, {"data", "Create category error."}];
	                        _Modle ->
	                            [{"success", true}, {"data", "ok."}]
	                    end
	            end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "list"], UserId) -> 
	Vals = yaws_api:parse_query(Arg),
	IncludeAll = erlang:list_to_atom(proplists:get_value("include_all", Vals, "false")),
	IncludeTrash = erlang:list_to_atom(proplists:get_value("include_trash", Vals, "false")),

	NoteCategories = model_nte_category:list(UserId, true, IncludeAll, IncludeTrash) ++ model_nte_share:category_list(UserId),
    CategoryList = [{struct, tools:record_to_list(NoteCategory, record_info(fields, note_category))} || NoteCategory <- NoteCategories],
	Result = [{"success", true}, {"data", {array, CategoryList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "mine", "list"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	AmendName = erlang:list_to_atom(proplists:get_value("amend_name", Vals)),

    NoteCategories = model_nte_category:list(UserId, AmendName, false, true),

    CategoryList = [{struct, tools:record_to_list(NoteCategory, record_info(fields, note_category))} || NoteCategory <- NoteCategories],

    Result = [{"success", true}, {"data", {array, CategoryList}}],

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "update"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),
	CategoryName = proplists:get_value("category_name", Vals),
	IsDefault = erlang:list_to_atom(proplists:get_value("is_default", Vals)),

	Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
                owner ->
                    case model_nte_category:exist(UserId, CategoryName, CategoryId) of
                        true -> [{"success", false}, {"data", "existing."}];
                        false ->
                            case model_nte_category:update(CategoryId, CategoryName, IsDefault, UserId) of
                                ok ->
                                    [{"success", true}, {"data", "ok."}];
                                error -> 
                                    [{"success", false}, {"data", "Update category error."}]
                            end
                    end;

                _ -> [{"success", false}, {"data", "You don't have permission to operate this category."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "delete"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),

	Result = case model_nte_category:delete(UserId, CategoryId) of
                ok ->
                    [{"success", true}, {"data", CategoryId}];
                last -> 
                    [{"success", false}, {"data", "last category."}];
                trash -> 
                    [{"success", false}, {"data", "trash category."}];
                error -> 
                    [{"success", false}, {"data", "Delete category error."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "display_order", "up"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),

	Result = case model_nte_category:up(UserId, CategoryId) of
                ok ->
                    [{"success", true}, {"data", CategoryId}];
                error -> 
                    [{"success", false}, {"data", "Up category error."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["category", "display_order", "down"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),

	Result = case model_nte_category:down(UserId, CategoryId) of
                ok ->
                    [{"success", true}, {"data", CategoryId}];
                error -> 
                    [{"success", false}, {"data", "Down category error."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


%% ==== share ========================================================

out(Arg, ["share", "add"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),
	SharedUserId = proplists:get_value("shared_user_id", Vals),
	Permission = erlang:list_to_atom(proplists:get_value("permission", Vals)),

	Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
                owner -> 
                    case model_usr_user:exist(SharedUserId) of
                        true ->
                            if
                                SharedUserId =:= UserId -> 
                                    [{"success", false}, {"data", "You can not add share to yourself."}];
                                true ->
                                    NoteShare = #nte_share{category_id=CategoryId, user_id=SharedUserId, share_type=Permission},
                                    case model_nte_share:create(NoteShare) of
                                        error -> [{"success", false}, {"data", "Operation error."}];
                                        duplicate -> [{"success", false}, {"data", "Share already exists."}];
                                        ok -> [{"success", true}, {"data", "ok."}]
                                    end
                            end;

                        false -> [{"success", false}, {"data", "The user you input doesn't exist."}]
                    end;

                _ -> [{"success", false}, {"data", "You don't have permission to share this category."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["share", "list"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	CategoryId = proplists:get_value("category_id", Vals),

	Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
                owner -> 
                    Shares = model_nte_share:list(CategoryId),
                    ShareList = [{struct, tools:record_to_list(Share, record_info(fields, note_share))} || Share <- Shares],
                    [{"success", true}, {"data", {array, ShareList}}];

                _ -> [{"success", false}, {"data", "You don't have permission to load the shares."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["share", "update"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ShareId = proplists:get_value("share_id", Vals),
	CategoryId = proplists:get_value("category_id", Vals),
	SharedUserId = proplists:get_value("shared_user_id", Vals),
	Permission = erlang:list_to_atom(proplists:get_value("permission", Vals)),

	Result = case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
                owner -> 
                    case model_usr_user:exist(SharedUserId) of
                        true ->
                            if
                                SharedUserId =:= UserId -> 
                                    [{"success", false}, {"data", "You can not add share to yourself."}];
                                true ->
                                    NoteShare = #nte_share{id=ShareId, category_id=CategoryId, user_id=SharedUserId, share_type=Permission},
                                    case model_nte_share:update(NoteShare) of
                                        ok -> [{"success", true}, {"data", "ok."}];
                                        error -> [{"success", false}, {"data", "Operation error."}]
                                    end
                            end;

                        false -> [{"success", false}, {"data", "The user you share to doesn't exist."}]
                    end;

                _ -> [{"success", false}, {"data", "You don't have permission to share this category."}]
            end,

    {content, "application/json", json2:encode({struct, Result})};


out(Arg, ["share", "delete"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ShareId = proplists:get_value("share_id", Vals),

	Result = case model_nte_share:get(ShareId) of
                error -> [{"success", false}, {"data", "The note share doesn't exist."}];
                NoteShare ->
                    case model_nte_share:get_permission_by_category_id(UserId, NoteShare#nte_share.category_id) of
                        owner ->
                            case model_nte_share:delete(ShareId) of
                                ok -> [{"success", true}, {"data", "ok."}];
                                error -> [{"success", false}, {"data", "Operation error."}]
                            end;

                        _ -> [{"success", false}, {"data", "You don't have permission to delete this note share."}]
                    end                    
            end,

    {content, "application/json", json2:encode({struct, Result})};


%% ==== share ========================================================

out(Arg, ["history", "list"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("note_id", Vals),

    Result = case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
                none -> [{"success", false}, {"data", "Don't have permission."}];
                r -> [{"success", false}, {"data", "Don't have permission."}];
                _ ->
                    case model_nte_history:list(NoteId) of
                        error ->
                            [{"success", false}, {"data", "Failed."}];
                        NoteHistoris ->
				            HistoryList = [{struct, tools:record_to_list(NoteHistory, record_info(fields, nte_history))} || NoteHistory <- NoteHistoris],
				            [{"success", true}, {"data", {array, HistoryList}}]
                    end
            end,

    {content, "application/json", json2:encode({struct, Result})};


%% ===================================================================
%% Mobile specific API
%% ===================================================================

out(Arg, ["update"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	NoteId = proplists:get_value("id", Vals),
	NoteContent = proplists:get_value("note", Vals),
	CategoryId = proplists:get_value("category_id", Vals),
	LastUpdated = proplists:get_value("last_updated", Vals),

	case model_nte_note:exist(NoteId) of
		true ->
			% update
		    case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
				none -> no_permission;
				r -> no_permission;
		        _ -> model_nte_note:update_note_content(NoteId, NoteContent, erlang:list_to_integer(LastUpdated), UserId)
		    end;
		false ->
			% create
			if 
				NoteId =:= "" -> 
					ActuallyCategoryId = case model_nte_category:exist(CategoryId) of
						true -> 
							case model_nte_share:get_permission_by_category_id(UserId, CategoryId) of
		                		owner -> CategoryId;
		                		rw -> CategoryId;
		                		_ -> (model_nte_category:get_default_or_any_category(UserId))#nte_category.id
		                	end;
		            	false -> (model_nte_category:get_default_or_any_category(UserId))#nte_category.id
		            end,

                    Note = #nte_note{category_id = ActuallyCategoryId, 
                                     note = NoteContent, 
                                     left = ?NOTE_DEFAULT_LEFT, 
                                     top = ?NOTE_DEFAULT_TOP, 
                                     width = ?NOTE_DEFAULT_WIDTH, 
                                     height = ?NOTE_DEFAULT_HEIGHT, 
                                     color = ?NOTE_DEFAULT_COLOR, 
                                     z_index = 1000},
                    model_nte_note:create(Note, UserId);

		        true -> note_was_deleted
		    end
	end,

	Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["deleteNotes"], UserId) ->
	Vals = yaws_api:parse_post(Arg),
	Ids = proplists:get_value("ids", Vals),

	NoteIds = string:tokens(Ids, ","),

	Fun = fun(NoteId) ->
		case model_nte_share:get_permission_by_note_id(UserId, NoteId) of
			none -> no_permission;
			r -> no_permission;
			_ -> 
            	case model_nte_note:get(NoteId) of
            		#nte_note{category_id = CategoryId} ->
            			#nte_category{attributes = Attributes} = model_nte_category:get(CategoryId),
            			case lists:member(trash, Attributes) of
            				true ->
            					%% delete it
								model_nte_note:delete(NoteId);
			                _ ->
			                	%% move to trash category
			                	model_nte_note:move_note_to_trash(NoteId, UserId)
			            end;
            		_ ->
            			already_deleted
            	end
		end
	end,

	lists:foreach(Fun, NoteIds),

	Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================

note_html() ->	
	{redirect_local, "/tools/note.html"}.
