-module(model_wrd_word).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/1,
		create/1,
		update/4,
		update_display_order/2,
		update_display_order/3,
		delete/1,
		list/1,
		is_owner/2]).


%% ===================================================================
%% API functions
%% ===================================================================

get(Id) ->
	Fun = fun() ->
			mnesia:read({wrd_word, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Word]} -> Word;
		_ -> error
	end.


create(Word) ->
	Id = tools:generate_id(Word#wrd_word.user_id),

	Fun = fun() ->
			mnesia:write(Word#wrd_word{id = Id,
								  display_order = tools:epoch_milli_seconds(),
								  last_updated = tools:epoch_milli_seconds()})	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


update(Id, Word, Pronunciation, Translation) ->
	case model_wrd_word:get(Id) of
		error -> error;
		WordModel ->
			Fun = fun() ->
				mnesia:write(WordModel#wrd_word{word = Word, 
										pronunciation = Pronunciation, 
										translation = Translation,
										last_updated = tools:epoch_milli_seconds()})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_display_order(Id, DisplayOrder) ->
	case model_wrd_word:get(Id) of
		error -> error;
		WordModel ->
			Fun = fun() ->
				mnesia:write(WordModel#wrd_word{display_order = DisplayOrder, 
										last_updated = tools:epoch_milli_seconds()})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_display_order(Id, DisplayOrder, LastUpdated) ->
	case model_wrd_word:get(Id) of
		error -> error;
		WordModel ->
			Fun = fun() ->
				if
					WordModel#wrd_word.last_updated < LastUpdated ->
						mnesia:write(WordModel#wrd_word{display_order = DisplayOrder, 
										               last_updated = tools:epoch_milli_seconds()});
					true -> ignore
				end
			end,
			mnesia:transaction(Fun), 
			ok
	end.


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({wrd_word, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


list(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(wrd_word),
				X#wrd_word.user_id =:= UserId]))
	end,

	{atomic, Words} = mnesia:transaction(Fun),
	

	SortFun = fun(A, B) ->
		A#wrd_word.display_order > B#wrd_word.display_order
	end,

	lists:sort(SortFun, Words).


is_owner(UserId, WordId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(wrd_word), 
						X#wrd_word.id =:= WordId, 
						X#wrd_word.user_id =:= UserId]))
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, [_Word]} -> true;
		_ -> false
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
