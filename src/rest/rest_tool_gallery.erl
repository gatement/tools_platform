-module(rest_tool_gallery).
-include("yaws_api.hrl").
-include("tools_platform.hrl").
-export([out/1]).

%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case erlang:is_record(Arg#arg.state, upload) of
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

    model_gly_item:create_album(AlbumName, UserId),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["item", "list"], UserId) ->
	Vals = yaws_api:parse_post(Arg),
	ParentId = proplists:get_value("parent_id", Vals),
	Height = proplists:get_value("height", Vals),

	ParentId2 = case ParentId of
		[] -> undefined;
		ParentId -> ParentId
	end,

    Items = model_gly_item:list(UserId, ParentId2),

    MapFun = fun(Item) ->
    	#gallery_item{id = Item#gly_item.id, 
					name = Item#gly_item.name, 
					thumbnail_url = lists:flatten(io_lib:format("/gallery/item/preview/~s/~s", [Item#gly_item.id, Height])),
					type = Item#gly_item.type,
					height = Height}
	end,
	Items2 = lists:map(MapFun, Items),

    ItemList = [{struct, tools:record_to_list(Item, record_info(fields, gallery_item))} || Item <- Items2],
	Result = [{"success", true}, {"data", {array, ItemList}}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, ["item", "preview", ItemId, _Height], _UserId) ->
	% todo: 
	% 1. check permission
	% 2. if Height is 0, return original img

	%HeightInt = erlang:list_to_integer(Height),

    Item = model_gly_item:get(ItemId),

	case Item#gly_item.type of
		"album" ->
			case Item#gly_item.path of
	    		undefined ->
	    			% todo: resize the defaultAlbumCover.jpg to specific height
	    			{ok, SiteDir0} = application:get_env(tools_platform, parent_dir),
	    			SiteDir = SiteDir0 ++ "/priv/site",
	    			Original = SiteDir ++ "/css/images/defaultAlbumCover.jpg",
	    			{ok, Binary} = file:read_file(Original),
					{content, "image/jpeg", Binary};
	    			%Thumbnail = string:strip(?GALLERY_THUMBNAIL_FOLDER, right, $/) ++ "/css/images/defaultAlbumCover.jpg",
	    			%filelib:ensure_dir(Thumbnail);

	    		_Path ->
	    			% todo: judge if Path exists, if exist, generate thumbnail
	    			% if doesn't exist, user default album cover
	    			todo
    		end;
    	"image" -> 
    		% todo: 
    		% 1. if doesn't exist, return a default ERROR image (user should delete such an image)
    		% 2. resize the image and return the image
    		todo;
    	"video" -> 
			case Item#gly_item.path of
	    		undefined ->
	    			% todo: resize the default video image to specific height
	    			"/css/images/defaultAlbumCover.jpg";
	    		_Path ->
	    			% todo: judge if Path exists, if exist, generate thumbnail
	    			% if doesn't exist, user default video cover
	    			todo
    		end
	end;

out(Arg, ["item", "upload"], _UserId) ->
	State = #upload{},
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
            case addFileChunk(Arg, Res, State#upload{last = true}) of
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

addFileChunk(_Arg, [], State) when State#upload.last == true,
                                   State#upload.filename /= undefined,
                                   State#upload.fd /= undefined ->
    file:close(State#upload.fd),
    Result = {ehtml, {p,[], "File upload done"}},
    {done, Result};

addFileChunk(_Arg, [], State) when State#upload.last == true ->
    {done, error()};

addFileChunk(_Arg, [], State) ->
    {cont, State};

addFileChunk(Arg, [{head, {_Name, Opts}} | Res], State ) ->
    case lists:keysearch("filename", 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),
		    {ok, GalleryOriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
		    file:make_dir(GalleryOriginalDir ++ "/"),
		    case file:open([GalleryOriginalDir, "/", Fname], [write]) of
				{ok, Fd} ->
				    S2 = State#upload{filename = Fname, fd = Fd},
				    addFileChunk(Arg, Res, S2);
				_Err ->
				    {done, error()}
		    end;
		false ->
	        addFileChunk(Arg,Res,State)
    end;

addFileChunk(Arg, [{body, Data} | Res], State) when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(Arg, Res, State);
        _Err ->
            {done, error()}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.
