-module(rest_tool_gallery).
-include("yaws_api.hrl").
-include("tools_platform.hrl").
-export([out/1]).

%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case erlang:is_record(Arg#arg.state, gly_item_upload) of
		true ->
			multipart(Arg, Arg#arg.state);
		false ->
			UserId = (Arg#arg.state)#arg_state.user_id,
			GalleryEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_GALLERY_ENABLED),

			case GalleryEnabled of
				true ->
					case Arg#arg.pathinfo of
						undefined -> 
							gallery_html();
						_ -> 
							out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
					end;

				false -> 
					{status, 404}
			end
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(Arg, ["album", "add"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	AlbumName = proplists:get_value("album_name", Vals),
	ParentId0 = proplists:get_value("parent_id", Vals),

	ParentId = case ParentId0 of
		[] -> undefined;
		_ -> ParentId0
	end,

    model_gly_item:create_album(AlbumName, ParentId, UserId),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};

out(Arg, ["item", "rename"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ItemId = proplists:get_value("item_id", Vals),
	ItemName = proplists:get_value("name", Vals),

    Result = case model_gly_item:rename(ItemId, ItemName, UserId) of
    	error ->
			[{"success", false}];
    	ok ->
    		[{"success", true}]
    end,

	{content, "application/json", json2:encode({struct, Result})};

out(Arg, ["item", "delete"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ItemIds0 = proplists:get_value("item_ids", Vals),

	ItemIds = string:tokens(ItemIds0, ","),
	Fails = [X || X <- ItemIds, model_gly_item:delete(X, UserId) =:= error],
	Result = [{"success", true}, {"failed_ids", {array, Fails}}],
	{content, "application/json", json2:encode({struct, Result})};

out(Arg, ["item", "move"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ItemIds0 = proplists:get_value("item_ids", Vals),
	TargetItemId = proplists:get_value("target_item_id", Vals),

	ItemIds = string:tokens(ItemIds0, ","),
	Fails = [X || X <- ItemIds, model_gly_item:move(X, TargetItemId, UserId) =:= error],
	Result = [{"success", true}, {"failed_ids", {array, Fails}}],
	{content, "application/json", json2:encode({struct, Result})};

out(_Arg, ["item", "info", ItemId], UserId) -> 
	ParentId = model_gly_item:get_parent_id(ItemId, UserId),
	Permission = model_gly_item:get_permission(ItemId, UserId),
	Info = #gallery_item_info{
		id = ItemId, 
		parent_id = ParentId, 
		permission = Permission
	},

	Result = [{"success", true}, {"data", {struct, tools:record_to_list(Info, record_info(fields, gallery_item_info))}}],
	{content, "application/json", json2:encode({struct, Result})};

out(Arg, ["item", "list"], UserId) ->
	Vals = yaws_api:parse_post(Arg),
	ParentId0 = proplists:get_value("parent_id", Vals),
	Height = proplists:get_value("height", Vals),

	case ParentId0 of
		[] -> 
			ParentId = undefined,

			%% if it is in root, also get all albums that shared to me
			SharedItemIds = model_gly_share:get_item_ids(UserId),
			SharedItems = [model_gly_item:get(X) || X <- SharedItemIds];
		_ ->
			ParentId = ParentId0,
			SharedItems = []
	end,

    Items = model_gly_item:list(UserId, ParentId, "album", false) ++ SharedItems ++ model_gly_item:list(UserId, ParentId, "image", true),
    Items2 = [#gallery_item{
					id = X#gly_item.id, 
					name = X#gly_item.name, 
					thumbnail_url = lists:flatten(io_lib:format("/gallery/item/preview/~s/~s", [X#gly_item.id, Height])),
					original_url = lists:flatten(io_lib:format("/gallery/item/preview/~s/0", [X#gly_item.id])),
					type = X#gly_item.type,
					height = Height}
				|| X <- Items],

    ItemList = [{struct, tools:record_to_list(Item, record_info(fields, gallery_item))} || Item <- Items2],
	Result = [{"success", true}, {"data", {array, ItemList}}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, ["item", "preview", ItemId, Height0], UserId) ->
	Height = erlang:list_to_integer(Height0),
    Item = model_gly_item:get(ItemId),

	case Item#gly_item.type of
		"album" ->
			case Item#gly_item.path of
	    		undefined ->
	    			%% orginal path
	    			{ok, SiteDir0} = application:get_env(tools_platform, parent_dir),
	    			OriginalFile = io_lib:format("~s/~s/~s", [SiteDir0, "priv/site", "css/images/galleryDefaultAlbumCover.jpg"]),
	    			MimeType = "image/jpeg",

	    			%% get thumbnail
	    			ThumbnailName = filename:basename(OriginalFile),
					get_thumbnail(OriginalFile, ThumbnailName, MimeType, Height, UserId);

	    		Path ->	    			
	    			%% orginal path
	    			{ok, OriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
					OriginalFile = io_lib:format("~s/~s/~s", [OriginalDir, UserId, Path]),

					case filelib:is_regular(OriginalFile) of
						true ->
							%% get thumbnail
			    			MimeType = Item#gly_item.mime_type,
			    			ThumbnailName = filename:basename(OriginalFile),
							get_thumbnail(OriginalFile, ThumbnailName, MimeType, Height, UserId);
						false ->
							%% if path doesn't exist, use the default album cover
			    			{ok, SiteDir0} = application:get_env(tools_platform, parent_dir),
			    			OriginalFile2 = io_lib:format("~s/~s/~s", [SiteDir0, "priv/site", "css/images/galleryDefaultAlbumCover.jpg"]),
			    			MimeType = "image/jpeg",

			    			%% get thumbnail
			    			ThumbnailName = filename:basename(OriginalFile2),
							get_thumbnail(OriginalFile2, ThumbnailName, MimeType, Height, UserId)
					end
    		end;
    	"image" ->
	    	%% orginal path
			{ok, OriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
			OriginalFile = io_lib:format("~s/~s/~s", [OriginalDir, UserId, Item#gly_item.path]),

			case filelib:is_regular(OriginalFile) of
				true ->
					%% get thumbnail
	    			MimeType = Item#gly_item.mime_type,
	    			ThumbnailName = filename:basename(OriginalFile),
					get_thumbnail(OriginalFile, ThumbnailName, MimeType, Height, UserId);
				false ->
					%% if path doesn't exist, use the default album cover
	    			{ok, SiteDir0} = application:get_env(tools_platform, parent_dir),
					OriginalFile2 = io_lib:format("~s/~s/~s", [SiteDir0, "priv/site", "css/images/galleryItemPathDeleted.jpg"]),
	    			MimeType = "image/jpeg",

	    			%% get thumbnail
	    			ThumbnailName = filename:basename(OriginalFile2),
					get_thumbnail(OriginalFile2, ThumbnailName, MimeType, Height, UserId)
			end;
    	"video" -> 
			case Item#gly_item.path of
	    		undefined ->
	    			% todo: resize the default video image to specific height
	    			"/css/images/galleryDefaultAlbumCover.jpg";
	    		_Path ->
	    			% todo: judge if Path exists, if exist, generate thumbnail
	    			% if doesn't exist, user default video cover
	    			todo
    		end
	end;

out(Arg, ["item", "upload", AlbumItemId], UserId) ->
	ItemId = tools:generate_id(UserId),
	
	State = #gly_item_upload{item_id = ItemId, album_item_id = AlbumItemId, user_id = UserId},
    multipart(Arg, State);

out(_Arg, _, _) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================

gallery_html() ->
	{redirect_local, "/tools/gallery.html"}.


error() ->
    {ehtml, {p, [], "error"}}.


get_thumbnail(OriginalFile, ThumbnailName, MimeType, Height, UserId) ->	
	%% thumbnail path
	{ok, ThumbnailDir0} = application:get_env(tools_platform, tool_gallery_thumbnail_dir),
	ThumbnailDir = io_lib:format("~s/~s/~p", [ThumbnailDir0, UserId, Height]),
	ThumbnailFile = io_lib:format("~s/~s", [ThumbnailDir, ThumbnailName]),

	case Height of
		0 ->
			%% return original
			{ok, Binary} = file:read_file(OriginalFile),
			{content, MimeType, Binary};
		_ ->
			%% generate thumbnail if necessary
			case filelib:is_regular(ThumbnailFile) of
				true ->
					do_nothing;
				false ->
					filelib:ensure_dir(ThumbnailFile),
					{ok, Exe} = application:get_env(tools_platform, imagemagick_exe),
					imagemagick:convert(Exe, OriginalFile, ThumbnailFile, Height)
			end,

			%% return thumbnail
			{ok, Binary} = file:read_file(ThumbnailFile),
			{content, MimeType, Binary}
	end.


multipart(Arg, State) ->
    Parse = yaws_api:parse_multipart_post(Arg),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(Arg, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(Arg, Res, State#gly_item_upload{last = true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    error()
            end;
        {error, _Reason} ->
            error()
    end.


addFileChunk(Arg, [{part_body, Data}|Res], State) ->
    addFileChunk(Arg, [{body, Data}|Res], State);

addFileChunk(_Arg, [], State) when State#gly_item_upload.last == true,
                                   State#gly_item_upload.file_full_name /= undefined,
                                   State#gly_item_upload.fd /= undefined ->
    file:close(State#gly_item_upload.fd),

    Type = lists:nth(1, string:tokens(State#gly_item_upload.mime_type, "/")),

    model_gly_item:create_item(#gly_item{
    		id = State#gly_item_upload.item_id, 
    		user_id = State#gly_item_upload.user_id, 
    		parent_id = State#gly_item_upload.album_item_id, 
    		name = State#gly_item_upload.item_name, 
    		path = State#gly_item_upload.path, 
    		type = Type,
    		mime_type = State#gly_item_upload.mime_type
    	}),

    Result = {ehtml, {p, [], "File upload done"}},
    {done, Result};

addFileChunk(_Arg, [], State) when State#gly_item_upload.last == true ->
    {done, error()};

addFileChunk(_Arg, [], State) ->
    {cont, State};

addFileChunk(Arg, [{head, {_Name, Opts}} | Res], State) ->
    case lists:keysearch("filename", 1, Opts) of
        {value, {_, FileName}} ->
        	%% generate path
			FileExtension = filename:extension(FileName),
			Date = tools:datetime_string('yyyyMMdd'),
			Path = lists:flatten(io_lib:format("~s/~s~s", [Date, State#gly_item_upload.item_id, FileExtension])),

        	%% generate physical full file name
			{ok, OriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
        	FileFullName = lists:flatten(io_lib:format("~s/~s/~s", [OriginalDir, State#gly_item_upload.user_id, Path])),
            filelib:ensure_dir(FileFullName),

		    case file:open(FileFullName, [write]) of
				{ok, Fd} ->
        			{value, {content_type, MimeType}} = lists:keysearch(content_type, 1, Opts),
				    State2 = State#gly_item_upload{item_name = FileName, file_full_name = FileFullName, mime_type = MimeType, path = Path, fd = Fd},
				    addFileChunk(Arg, Res, State2);
				_Err ->
				    {done, error()}
		    end;
		false ->
	        addFileChunk(Arg,Res,State)
    end;

addFileChunk(Arg, [{body, Data} | Res], State) when State#gly_item_upload.file_full_name /= undefined ->
    case file:write(State#gly_item_upload.fd, Data) of
        ok ->
            addFileChunk(Arg, Res, State);
        _Err ->
            {done, error()}
    end.
