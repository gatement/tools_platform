-module(rest_tool_word).
-include("yaws_api.hrl").
-include("tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,		
	WordEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_WORD_ENABLED),
	case WordEnabled of
		true ->
			case Arg#arg.pathinfo of
				undefined -> 
					word_yaws();
				_ -> 
					out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
			end;

		false -> {status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(Arg, ["create"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	Word = proplists:get_value("word", Vals),
	Pronunciation = proplists:get_value("pronunciation", Vals),
	Translation = proplists:get_value("translation", Vals),

	WordModel = #wrd_word{user_id = UserId, 
						word = Word, 
						pronunciation = Pronunciation, 
						translation = Translation},

	Result = case model_wrd_word:create(WordModel) of
		ok -> [{"success", true}, {"data", ""}];
		error -> [{"success", false}, {"data", "Adding word error."}]
	end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["update"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	WordId = proplists:get_value("id", Vals),
	Word = proplists:get_value("word", Vals),
	Pronunciation = proplists:get_value("pronunciation", Vals),
	Translation = proplists:get_value("translation", Vals),

	Result = case model_wrd_word:is_owner(UserId, WordId) of
		true -> 
			model_wrd_word:update(WordId, Word, Pronunciation, Translation),
			[{"success", true}, {"data", ""}];
		false -> [{"success", false}, {"data", "You are not the owner of this word."}]
	end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["updateWordsPosition"], UserId) ->
	Vals = yaws_api:parse_post(Arg),

	Fun = fun({WordId, Data}) ->
		case model_wrd_word:is_owner(UserId, WordId) of
			true -> 
				[DisplayOrder, LastUpdated] = string:tokens(Data, ","),
				model_wrd_word:update_display_order(WordId, erlang:list_to_integer(DisplayOrder), LastUpdated);
			false -> false
		end
	end,

	lists:foreach(Fun, Vals),

	Result = [{"success", true}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["delete"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	WordId = proplists:get_value("id", Vals),

	Result = case model_wrd_word:is_owner(UserId, WordId) of
		true -> 
			model_wrd_word:delete(WordId),
			[{"success", true}, {"data", ""}];
		false -> [{"success", false}, {"data", "You are not the owner of this word."}]
	end,

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["deleteWords"], UserId) ->
	Vals = yaws_api:parse_post(Arg),
	Ids = proplists:get_value("ids", Vals),

	WordIds = string:tokens(Ids, ","),

	Fun = fun(WordId) ->
		case model_wrd_word:is_owner(UserId, WordId) of
			true -> model_wrd_word:delete(WordId);
			false -> noaction
		end
	end,

	lists:foreach(Fun, WordIds),

	Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, ["list"], UserId) -> 

	Words = model_wrd_word:list(UserId),
	WordList = [{struct, tools:record_to_list(Word, record_info(fields, wrd_word))} || Word <- Words],
    Result = [{"success", true}, {"data", {array, WordList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["sort"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),

	Fun = fun({WordId, DisplayOrder}) ->
		case model_wrd_word:is_owner(UserId, WordId) of
			true -> model_wrd_word:update_display_order(WordId, erlang:list_to_integer(DisplayOrder));
			false -> false
		end
	end,

	lists:foreach(Fun, Vals),

	Result = [{"success", true}, {"data", ""}],

	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _, _) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================

word_yaws() ->		
	{redirect_local, "/word.yaws"}.
