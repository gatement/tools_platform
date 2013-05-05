if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Word()
	{
		var me = this;

		$("#add_word").focus();
		
		$("#add_submit").click(function(){me.addWord()});
		$(document).keydown(function(event) 
		{ 
			if (event.keyCode == 13 && event.ctrlKey) 
			{ 
				me.addWord();
			} 
		});

		$("#show_word").change(function(){me.showWord()});
		$("#show_translation").change(function(){me.showTranslation()});

		$("#wordListDiv").sortable(
		{
			change : function() 
			{
				$(".save_sorting_button").fadeIn();
			}
		});
		$("#wordListDiv").disableSelection();
		$(".save_sorting_button").click(function(){me.sortList()}).hide();
	};
	

	$.extend(Word.prototype, {
		init: function()
		{
			this.refreshList();
		},

		addWord: function()
		{
			var me = this;

			var word = $.trim($("#add_word").val());
			if (!word) {
				$("#message").text("Word is required.");
				$("#add_word").focus();
				return;
			}

			var pronunciation = ""
			var translation = "";

			// word[pronunciation]\ntranslation
			var arr = word.split("\n");
			var arrWord = arr[0].split("[");

			var word = arrWord[0];
			var pronunciation = ""

			if (arrWord.length > 1) {
				arrWord.shift();
				pronunciation = "[" + arrWord.join("[");
			}

			if (arr.length > 1) {
				arr.shift();
				translation = arr.join("\n");
			}

			// post data
			var data = {
				"word" : word,
				"pronunciation" : pronunciation,
				"translation" : translation
			};

			var url = "/word/create";

			var id = $("#add_id").val();
			if (id)
			{
				data.id = id;
				url = url = "/word/update";
			}

			this.ajax(url, data, function(response) 
			{
				if(response.success)
				{
					me.refreshList();

					// clear data
					$("#add_id").val("");
					$("#add_word").val("");
					$("#message").text("");

					$("#add_word").focus();
				}
				else
				{
					window.alert(response.data);
				}
			}, null, "POST");

		},

		refreshList: function()
		{
			var me = this;

			var url = "/word/list";
			this.ajax(url, {}, function(response) 
			{
				if(response.success)
				{
					me.bindList(response.data);
					$("#show_word").attr("checked", "checked");
					$("#show_translation").attr("checked", "checked");
				}
				else
				{
					window.alert(response.data);
				}
			}, null, "GET");
		},

		bindList: function(data)
		{
			var me = this;

			$("#wordItemTemplate").tmpl(data, {
				dataSource : data
			}).appendTo($("#wordListDiv").empty());

			$(".edit_button").click(function(event){me.editButtonClick(event)});
			$(".remove_button").click(function(event){me.removeButtonClick(event)});

			$(".save_sorting_button").fadeOut();
		},

		editButtonClick: function(event)
		{
			var id = $(event.target).parent().next().val();
			var word = $(event.target).parent().parent().children().first().find(".wordItem_word").first().text();
			var pronunciation = $(event.target).parent().parent().children().first().find(".wordItem_pronunciation").first().text();
			var translation = $(event.target).parent().siblings(".wordItem_translation").first().text();

			word = word + pronunciation + "\n" + translation;

			$("#add_id").val(id);
			$("#add_word").val(word);

			$("html, body").animate({ scrollTop: 0 }, "fast");
		},

		removeButtonClick: function(event)
		{
			var me = this;

			var id = $(event.target).parent().next().val();
			var data = 
			{
				"id" : id
			};

			var url = "/word/delete";

			this.ajax(url, data, function(response) 
			{
				if(response.success)
				{
					me.refreshList();
				}
				else
				{
					window.alert(response.data);
				}
			}, null, "POST");
		},

		showWord: function()
		{
			if ($("#show_word")[0].checked) 
			{
				$(".wordItem_word_pronunciation").fadeIn();
			} 
			else 
			{
				$(".wordItem_word_pronunciation").fadeOut();

				// check the other one
				if (!$("#show_translation").attr("checked")) 
				{
					$("#show_translation").attr("checked", "checked").change();
				}
			}
		},

		showTranslation: function()
		{
			if ($("#show_translation")[0].checked) 
			{
				$(".wordItem_translation").fadeIn();
			} 
			else 
			{
				$(".wordItem_translation").fadeOut();

				// check the other one
				if (!$("#show_word").attr("checked")) 
				{
					$("#show_word").attr("checked", "checked").change();
				}
			}
		},

		sortList: function() 
		{
			var me = this;

			var max = $(".wordListItem").length;
			var data = {};

			$(".wordListItem").each(function() 
			{
				var id = $(this).find(".wordItemId").val();

				data[id] = max--;
			});

			var url = "/word/sort";

			this.ajax(url, data, function(response) 
			{
				if(response.success)
				{
					me.refreshList();
				}
				else
				{
					window.alert(response.data);
				}
			}, null, "POST");
		},

		ajax: function(url, data, success, error, method) 
		{
			data = data || {};
			method = method ? method : "POST";

			$.ajax({
				    url: url,
				    type: method,
				    data: data,
				    success: function (data, textStatus, jqXHR) 
		            {
		            	if(success) success(data);
				    },
				    error: function (jqXHR, textStatus, errorThrown) 
		            {		        
				        if(error) error();
				    }
				});
		}

	});

	tp.Word = Word;
	
})(jQuery, tp);



$(document).ready(function() 
{
	tp.word = new tp.Word();
	tp.word.init();
});

