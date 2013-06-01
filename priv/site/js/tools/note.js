if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Note()
	{
		this.firstLoading = true;

		this.WebSocket = null;
		this.webSocket = null;

		this.loadCategoriesDeferred = $.Deferred();
		this.z_index = 0;
		this.selectedCategoryId = null;
		this.currentNoteId = "";

		this.arrangeNotes = {};

		this.arrangeNotes.defaultLeft = 5;
		this.arrangeNotes.defaultTop = 39;

		this.arrangeNotes.defaultWidth = 220;
		this.arrangeNotes.defaultHeight = 170;

		this.arrangeNotes.defaultMinHeight = 18;
		this.arrangeNotes.defaultMinWidth = 190;

		this.arrangeNotes.defaultActualWidth = 240;
		this.arrangeNotes.defaultActualHeight = 203;

		this.arrangeNotes.defaultActualMinWidth = 210;
		this.arrangeNotes.defaultActualMinHeight = 51;

		this.arrangeNotes.increaseStepLeft = 10;
		this.arrangeNotes.increaseStepTop = 10;
		this.arrangeNotes.horizontalGap = 4;
		this.arrangeNotes.verticalGap = 4;

		this.autoSavingTimeout = {};
		this.autoSavingTimeoutVal = 5000;
		this.errorMsgTimeout = {};
		this.errorMsgTimeoutVal = 8000;
		this.errorRetryingInterval = 10000;

		var me = this;

		this.loadCategoriesDeferred.done(function(){
			$("#add").click(function(){me.add_note()});
			$("#tile_notes").click(function(){me.tile_notes_locally()});
			$("#list_notes").click(function(){me.list_notes_locally()});
			$("#list_small_notes").click(function(){me.list_small_notes_locally()});
			$("#list_left").click(function(){me.list_left_locally()});
			$("#list_right").click(function(){me.list_right_locally()});
			$("#list_top").click(function(){me.list_top_locally()});
			$("#list_bottom").click(function(){me.list_bottom_locally()});
			$("#refresh").click(function(){me.load_notes()});

			$("#noteCategory").change(function()
			{
				me.load_notes();
			});

			$("#search").focus(function()
			{
				if($("#search").val() === "input text to search")
				{
					$("#search").val("");
				}
			}).blur(function()
			{
				if($.trim($("#search").val()) === "")
				{
					$("#search").val("input text to search");
				}
			}).keyup(function()
			{
				me.search_notes();
			});

			$("#categoryMgmt").click(function(){me.open_category_management_dialog()});
			$("#shareMgmt").click(function(){me.open_category_share_dialog()});
			$("#categoryMgmtAddButton").click(function(){me.add_category()});
			$("#shareMgmtAddButton").click(function(){me.add_note_share()});
			$("#noteCategorySelect").change(function()
			{
				$("#noteCategoryDialog").dialog("close");
				me.update_note_category();
			});
		});
		
		$("#logout").click(function(){me.logout()});
		$("#about").click(function(){me.about()});
		$("#homepage").click(function(){me.homepage()});
	};

	$.extend(Note.prototype, {
		init: function()
		{
			if(this.firstLoading)
			{
				this.load_categories();
				this.firstLoading = false;
			}
		},










































		//============ toolbar operations ==============================================================
		logout: function() 
		{
			var successFunc = function()
			{
				window.location = "/login.yaws";
			}; 

		    this.ajax("/user/logout/noredirect", {}, successFunc, null, 'POST');
		},

		homepage: function()
		{
			window.location="/";
		},

		about: function()
		{
			window.alert("Author: Johnson Lau\n" +
				  "Email: gatement@gmail.com");
		},



























		//============ IO ==========================================================================

		send_msg: function(url, data, successFunc, errorFunc, method)
		{
			var me = this;

			data = data || {};
			method = method ? method : "POST";

			$.ajax({
				    url: url,
				    type: method,
				    data: data,
				    success: function (response, textStatus, jqXHR) 
		            {
						if(response.success === true)
						{
							if(successFunc) successFunc(response.data);
						}
						else if(response.success === false)
						{
							if(errorFunc) errorFunc(response.data);
						}
						else
						{
							me.set_error_msg("Session timeout, please login again.", true);
						}
				    },
				    error: function (jqXHR, textStatus, errorThrown) 
		            {
						me.set_error_msg("Communication error, is retrying...");
						window.setTimeout(function(){me.send_msg(url, data, successFunc, errorFunc, method);}, me.errorRetryingInterval);
				    }
				});
		},

		ajax: function(url, data, successFunc, errorFunc, method) 
		{
			data = data || {};
			method = method ? method : "POST";

			$.ajax({
				    url: url,
				    type: method,
				    data: data,
				    success: function (data, textStatus, jqXHR) 
		            {
		            	if(successFunc) successFunc(data);
				    },
				    error: function (jqXHR, textStatus, errorThrown)
		            {		        
				        if(errorFunc) errorFunc();
				    }
				});
		},






















		//============ web socket operations ===========================================================

		add_note: function() 
		{
			var me = this;

			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
			    var data = {category_id: $("#noteCategory").val(), z_index: ++this.z_index};

			    var successFunc = function(data)
				{
					me.show_note(data);
					me.bind_events();
					$("#" + data.id).find(".body").focus();
				};

				var errorFunc = function(data) 
				{
					me.set_error_msg(data);
				};

				this.send_msg("/note/note/add", data, successFunc, errorFunc);
			}
			else
			{
				window.alert("You don't have permission to add note to current category.");
			}
		},

		load_notes: function() 
		{
			var me = this;

			$("#notes").empty();
			$("#search").val("input text to search");

			var data = {};

			var successFunc = function(data) 
			{
				for(var i = 0; i < data.length; i++)
				{
					me.show_note(data[i]);
				}

				me.bind_events();
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			var url = "/note/note/list?category_id=" + $("#noteCategory").val();
			this.send_msg(url, data, successFunc, errorFunc);
		},

		search_notes: function()
		{
			var me = this;
			var key = $("#search").val().toLowerCase();

			var notes=[];
			$notes = $(".note");
			for(var i = 0; i < $notes.length; i++)
			{
				var $note = $($notes[i]);
				if($note.find("textarea").val().toLowerCase().indexOf(key) == -1)
				{
					$note.hide();
				}
				else
				{
					$note.show();
				}
			}
		},

		load_categories: function() 
		{
			var me = this;

			this.selectedCategoryId = $("#noteCategory").val();

			$("#notes").empty();
			$("#noteCategory").empty();
			$("#search").val("input text to search");

			var data = {};

			var successFunc = function(data) 
			{
				for(var i = 0; i < data.length; i++)
				{
					var item = data[i];
					var option = null;		
					if((me.selectedCategoryId === null && item.is_default) || item.id === me.selectedCategoryId) 
					{
						option = $.utils.formatStr('<option value="{0}" data-permission=\"{1}\" selected=\"selected\">{2}</option>',  item.id, item.permission, item.name);
					}
					else
					{
						option = $.utils.formatStr('<option value="{0}" data-permission=\"{1}\">{2}</option>',  item.id, item.permission, item.name);
					}
					
					$("#noteCategory").append(option);
				}

				me.load_notes();
				me.loadCategoriesDeferred.resolve();
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			var url = "/note/category/list?include_all=true&include_trash=true";
			this.send_msg(url, data, successFunc, errorFunc);
		},

		update_note_category: function() 
		{
			var me = this;

			var newCategoryId = $("#noteCategorySelect").val();
			var oldCategoryId = $("#noteCategory").val();

			if(newCategoryId !== oldCategoryId)
			{
				$("#"+this.currentNoteId).remove();

			    var data = {note_id: this.currentNoteId, new_category_id: newCategoryId};

				var successFunc = function(data) { };

				var errorFunc = function(data) 
				{
					me.set_error_msg(data);
				};

				this.send_msg("/note/note/update/category", data, successFunc, errorFunc);
			}
		},

		update_note_size: function(noteId, width, height) 
		{
			var me = this;

		    var data = {note_id: noteId, width: width, height: height};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/update/size", data, successFunc, errorFunc);
		},

		update_note_position: function(noteId, left, top) 
		{
			var me = this;

		    var data = {note_id: noteId, left: left, top: top};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/update/position", data, successFunc, errorFunc);
		},

		update_note_z_index: function(noteId, z_index) 
		{
			var me = this;

		    var data = {note_id: noteId, z_index: z_index};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/update/z_index", data, successFunc, errorFunc);
		},

		update_note_color: function(noteId, color) 
		{
			var me = this;

		    var data = {note_id: noteId, color: color};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/update/color", data, successFunc, errorFunc);
		},

		update_note_content: function(noteId) 
		{
			var me = this;

			var $note = $("#" + noteId);
			var content = $note.find(".body").val();

		    var data = {note_id: noteId, content: content};

			var successFunc = function(data) 
			{
				me.hide_writing_icon(noteId);
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/update/content", data, successFunc, errorFunc);
		},

		delete_note: function(noteId) 
		{
			var me = this;

			$("#" + noteId).remove();

		    var data = {note_id: noteId};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/delete", data, successFunc, errorFunc);
		},

		arrange_notes: function(notes) 
		{
			var me = this;

		    var data = {notes: notes};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/note/arrange/notes", data, successFunc, errorFunc);
		},

		add_category: function() 
		{
			var me = this;

			var categoryName = $.trim($("#categoryMgmtAddTextbox").val());
			var isDefault = $("#categoryMgmtAddDefaultCheckbox")[0].checked;
			
			if(categoryName)
			{
			    var data = {category_name: categoryName, is_default: isDefault};

				var successFunc = function(data) 
				{
					$("#categoryMgmtAddTextbox").val("");
					$("#categoryMgmtAddDefaultCheckbox").attr("checked", false);

					me.load_my_categories();
				};

				var errorFunc = function(data) 
				{
					if(data === "existing.")
					{
						window.alert("Category name already exists, please input another name.");
					}
					else
					{
						me.set_error_msg(data);
					}
				};

				this.send_msg("/note/category/add", data, successFunc, errorFunc);				
		    }
		},

		load_my_categories: function() 
		{
			var me = this;

			var $categoryMgmtList =	$("#categoryMgmtList");

			$categoryMgmtList.empty();

			var data = {amend_name: false};

			var successFunc = function(data) 
			{
				for(var i = 0; i < data.length; i++)
				{
					$("#categoryMgmtListItemTemplate").tmpl(data[i]).appendTo($categoryMgmtList);
				}

				$(".categoryMgmtListItemSaveButton").click(function(event){me.update_category(event)});
				$(".categoryMgmtListItemDeleteButton").click(function(event){me.delete_category(event)});
				$(".categoryMgmtListItemUpButton").click(function(event){me.up_category(event)});
				$(".categoryMgmtListItemDownButton").click(function(event){me.down_category(event)});
			};

			var errorFunc = function(data) { };

			this.send_msg("/note/category/mine/list", data, successFunc, errorFunc);
		},

		load_note_categories: function() 
		{
			var me = this;

			$("#noteCategorySelect").empty();

			var data = {amend_name: true};

			var successFunc = function(data) 
			{
				var $noteCategorySelect = $("#noteCategorySelect");			
				var categoryId = $("#noteCategory").val();

				for(var i = 0; i < data.length; i++)
				{
					var item = data[i];
					var option = null;

					if(item.id === categoryId) 
					{
						option = $.utils.formatStr('<option value="{0}" selected=\"selected\">{1}</option>',  item.id, item.name);
					}
					else
					{
						option = $.utils.formatStr('<option value="{0}">{1}</option>',  item.id, item.name);
					}

					$noteCategorySelect.append(option);
				}
			};

			var errorFunc = function(data) { };

			this.send_msg("/note/category/mine/list", data, successFunc, errorFunc);
		},

		load_note_history: function(noteId) 
		{
			var me = this;

			var $noteHistoryList = $("#noteHistoryList");			
			$noteHistoryList.empty();

			var data = {note_id: noteId};

			var successFunc = function(data) 
			{
				for(var i = 0; i < data.length; i++)
				{
					$("#noteHistoryListItemTemplate").tmpl(data[i]).appendTo($noteHistoryList);
				}
			};

			var errorFunc = function(data) { };

			this.send_msg("/note/history/list", data, successFunc, errorFunc);
		},

		update_category: function(event) 
		{
			var me = this;

			var $categoryTextbox = $(event.target).parent().find(".categoryMgmtListItemTextbox");
			var categoryId = $categoryTextbox.attr("data-id");
			var categoryName = $.trim($categoryTextbox.val());
			var isDefault = $(event.target).parent().find(".categoryMgmtListItemDefaultCheckbox")[0].checked;
			
			var data = {category_id: categoryId, category_name: categoryName, is_default: isDefault};

			var successFunc = function(data) 
			{
				me.load_my_categories();
			};

			var errorFunc = function(data) 
			{
				if(data === "existing.")
				{
					window.alert("Category name already exists, please input another name.");
				}
				else
				{
					me.set_error_msg(data);
				}
			};

			this.send_msg("/note/category/update", data, successFunc, errorFunc);
		},

		delete_category: function(event) 
		{
			var me = this;

			if(window.confirm("Confirm deleting the category? Notes under it will be moved to the default category."))
			{
				var $categoryTextbox = $(event.target).parent().find(".categoryMgmtListItemTextbox");
				var categoryId = $categoryTextbox.attr("data-id");

			    var data = {category_id: categoryId};

				var successFunc = function(data) 
				{
					var categoryId = data;
					$(".categoryMgmtListItemTextbox[data-id=" + categoryId + "]").parent().remove();
				};

				var errorFunc = function(data) 
				{
					if(data === "last category.")
					{
						window.alert("You can not delete the last category.");
					}
					else if(data === "trash category.")
					{
						window.alert("You can not delete the trash category.");
					}
					else
					{
						me.set_error_msg(data);
					}
				};

				this.send_msg("/note/category/delete", data, successFunc, errorFunc);
			}
		},

		up_category: function(event)
		{
			var me = this;

			var $categoryTextbox = $(event.target).parent().find(".categoryMgmtListItemTextbox");
			var categoryId = $categoryTextbox.attr("data-id");

			var data = {category_id: categoryId};

			var successFunc = function(data) 
			{
				me.load_my_categories();
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/category/display_order/up", data, successFunc, errorFunc);
		},

		down_category: function(event)
		{
			var me = this;

			var $categoryTextbox = $(event.target).parent().find(".categoryMgmtListItemTextbox");
			var categoryId = $categoryTextbox.attr("data-id");

		    var data = {category_id: categoryId};

			var successFunc = function(data) 
			{
				me.load_my_categories();
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/category/display_order/down", data, successFunc, errorFunc);
		},

		add_note_share: function() 
		{
			var me = this;

			var sharedUserId = $.trim($("#shareMgmtAddTextbox").val());
			var categoryId = $("#noteCategory").val();
			var permission = $("#shareMgmtAddSelect").val();

			if(sharedUserId)
			{
			    var data = {shared_user_id: sharedUserId, category_id: categoryId, permission: permission};

				var successFunc = function(data) 
				{
					$("#shareMgmtAddTextbox").val("");
					me.load_note_shares();
				};

				var errorFunc = function(data) 
				{
					if(data === "Share already exists."
						|| data === "The user you input doesn't exist."
						|| data === "You can not add share to yourself.")
					{
						window.alert(data);
					}
					else
					{
						me.set_error_msg(data);
					}
				};

				this.send_msg("/note/share/add", data, successFunc, errorFunc);
		    }
		},

		load_note_shares: function() 
		{
			var me = this;

			$("#shareMgmtList").empty();
			var categoryId = $("#noteCategory").val();

			if(categoryId)
			{
				var data = {category_id: categoryId};

				var successFunc = function(data) 
				{
					var $shareMgmtList = $("#shareMgmtList");

					for(var i = 0; i < data.length; i++)
					{
						$("#shareMgmtListItemTemplate").tmpl(data[i]).appendTo($shareMgmtList);
					}

					$(".shareMgmtListItemSaveButton").click(function(event){me.update_note_share(event)});
					$(".shareMgmtListItemDeleteButton").click(function(event){me.delete_note_share(event)});
				};

				var errorFunc = function(data) 
				{
					me.set_error_msg(data);
				};

				this.send_msg("/note/share/list", data, successFunc, errorFunc);
		    }
		},

		update_note_share: function(event) 
		{
			var me = this;

			var categoryId = $("#noteCategory").val();
			var $shareUser = $(event.target).parent().find(".shareMgmtListItemTextbox");
			var shareId = $shareUser.attr("data-share-id");
			var sharedUserId = $shareUser.attr("data-user-id");
			var permission = $(event.target).parent().find(".shareMgmtSelect").val();

			var data = {share_id: shareId, shared_user_id: sharedUserId, category_id: categoryId, permission: permission};

			var successFunc = function(data) { };

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/share/update", data, successFunc, errorFunc);
		},

		delete_note_share: function(event) 
		{
			var me = this;

			var shareId = $(event.target).parent().find(".shareMgmtListItemTextbox").attr("data-share-id");
			
			var data = {share_id: shareId};

			var successFunc = function(data) 
			{
				me.load_note_shares();
			};

			var errorFunc = function(data) 
			{
				me.set_error_msg(data);
			};

			this.send_msg("/note/share/delete", data, successFunc, errorFunc);
		},
































		//============ helpers =========================================================================
		open_category_management_dialog: function()
		{
			var me = this;
			this.load_my_categories();
			$("#categoryMgmtDialog").dialog({modal: true, width: 620, minWidth: 620, close: function(event, ui) {me.load_categories();}});
			$("#categoryMgmtAddTextbox").focus();
		},

		open_category_share_dialog: function()
		{
			if(this.get_current_category_permission() === "owner")
			{
				this.load_note_shares();
				$("#shareMgmtDialog").dialog({modal: true, width: 500, minWidth: 500});
				$("#shareMgmtAddTextbox").focus();
			}
			else
			{
				window.alert("You don't have permission to edit current category share.");
			}
		},

		open_note_category_dialog: function(x, y)
		{
			this.load_note_categories();
			$("#noteCategoryDialog").dialog({modal: true, width: 230, minWidth: 230, minHeight: 80, position: [x, y]});
		},

		open_note_history_dialog: function(noteId)
		{
			if(this.get_current_category_permission() === "owner")
			{
				this.load_note_history(noteId);
				$("#noteHistoryDialog").dialog({modal: true, width: 500, minWidth: 500});
			}
			else
			{
				window.alert("You don't have permission to view note history.");
			}
		},

		get_current_category_permission: function()
		{
			var currentCategoryVal = $("#noteCategory").val();
			var currentCategory = $("#noteCategory option[value="+currentCategoryVal+"]");

			return currentCategory.attr("data-permission");
		},

		tile_notes_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(true);

			// set note position values
			var left = this.arrangeNotes.defaultLeft;
			var top = this.arrangeNotes.defaultTop;
			for(var i = 0; i < notes.length; i++)
			{
				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = -1;
				note.height = -1;

				// update UI
				$("#"+note.id).animate({left: left, top: top});

				left += this.arrangeNotes.increaseStepLeft;
				top += this.arrangeNotes.increaseStepTop;
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_notes_locally: function()
		{
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			for(var i = 0; i < notes.length; i++)
			{
				left = this.get_list_notes_left(i);
				top = this.get_list_notes_top(i);
			    width = this.arrangeNotes.defaultWidth;
			    height = this.arrangeNotes.defaultHeight;

				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_small_notes_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			for(var i = 0; i < notes.length; i++)
			{
				left = this.get_list_small_notes_left(i);
				top = this.get_list_small_notes_top(i);
			    width = this.arrangeNotes.defaultMinWidth;
			    height = this.arrangeNotes.defaultMinHeight;

				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_left_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			var emphasizingNote = notes[0];

			left = this.arrangeNotes.defaultLeft + this.arrangeNotes.defaultActualWidth + this.arrangeNotes.horizontalGap;
			top = this.arrangeNotes.defaultTop;
			width = this.get_client_width() - left - 20 - this.arrangeNotes.horizontalGap; // 20 is the note horizontal margin+border+padding
			height = this.get_client_height() - this.arrangeNotes.defaultTop - 33 - this.arrangeNotes.verticalGap; // 33 is the note vertical margin+border+padding

			emphasizingNote.left = left;
			emphasizingNote.top = top;
			emphasizingNote.width = width;
			emphasizingNote.height = height;

			// update UI
			$("#"+emphasizingNote.id).animate({left: left, top: top, width: width, height: height});
			

			left = this.arrangeNotes.defaultLeft;
			width = this.arrangeNotes.defaultWidth;
			height = this.arrangeNotes.defaultMinHeight;

			for(var i = 1; i < notes.length; i++)
			{
				top = this.arrangeNotes.defaultTop + (this.arrangeNotes.defaultActualMinHeight + this.arrangeNotes.verticalGap) * (i - 1);
	
				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_right_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			var emphasizingNote = notes[0];

			left = this.arrangeNotes.defaultLeft;
			top = this.arrangeNotes.defaultTop;
			width = this.get_client_width() - this.arrangeNotes.defaultLeft - 20 - this.arrangeNotes.horizontalGap - this.arrangeNotes.defaultActualWidth - this.arrangeNotes.horizontalGap; // 20 is the note horizontal margin+border+padding of the large note
			height = this.get_client_height() - this.arrangeNotes.defaultTop - 33 - this.arrangeNotes.verticalGap; // 33 is the note vertical margin+border+padding

			emphasizingNote.left = left;
			emphasizingNote.top = top;
			emphasizingNote.width = width;
			emphasizingNote.height = height;

			// update UI
			$("#"+emphasizingNote.id).animate({left: left, top: top, width: width, height: height});
			

			left = this.get_client_width() - this.arrangeNotes.horizontalGap - this.arrangeNotes.defaultActualWidth;
			width = this.arrangeNotes.defaultWidth;
			height = this.arrangeNotes.defaultMinHeight;

			for(var i = 1; i < notes.length; i++)
			{
				top = this.arrangeNotes.defaultTop + (this.arrangeNotes.defaultActualMinHeight + this.arrangeNotes.verticalGap) * (i - 1);
	
				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_top_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			var emphasizingNote = notes[0];

			left = this.arrangeNotes.defaultLeft;
			top = this.arrangeNotes.defaultTop + this.arrangeNotes.defaultActualMinHeight + this.arrangeNotes.verticalGap;
			width = this.get_client_width() - this.arrangeNotes.defaultLeft - 20 - this.arrangeNotes.horizontalGap; // 20 is the note horizontal margin+border+padding of the large note
			height = this.get_client_height() - this.arrangeNotes.defaultTop - this.arrangeNotes.defaultActualMinHeight - this.arrangeNotes.verticalGap - 33 - this.arrangeNotes.verticalGap; // 33 is the note vertical margin+border+padding of large note

			emphasizingNote.left = left;
			emphasizingNote.top = top;
			emphasizingNote.width = width;
			emphasizingNote.height = height;

			// update UI
			$("#"+emphasizingNote.id).animate({left: left, top: top, width: width, height: height});
			

			top = this.arrangeNotes.defaultTop;
			width = this.arrangeNotes.defaultWidth;
			height = this.arrangeNotes.defaultMinHeight;

			for(var i = 1; i < notes.length; i++)
			{
				left = this.arrangeNotes.defaultLeft + (this.arrangeNotes.defaultActualWidth + this.arrangeNotes.horizontalGap) * (i - 1);
	
				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		list_bottom_locally: function()
		{
			// get note array
			var notes = this.get_sorted_notes(false);

			// set note position/size values
			var left = 0;
			var top = 0;
			var width = 0;
			var height = 0;

			var emphasizingNote = notes[0];

			left = this.arrangeNotes.defaultLeft;
			top = this.arrangeNotes.defaultTop;
			width = this.get_client_width() - this.arrangeNotes.defaultLeft - 20 - this.arrangeNotes.horizontalGap; // 20 is the note horizontal margin+border+padding of the large note
			height = this.get_client_height() - this.arrangeNotes.defaultTop - this.arrangeNotes.defaultActualMinHeight - 33 - this.arrangeNotes.verticalGap - this.arrangeNotes.verticalGap; // 33 is the note vertical margin+border+padding of large note

			emphasizingNote.left = left;
			emphasizingNote.top = top;
			emphasizingNote.width = width;
			emphasizingNote.height = height;

			// update UI
			$("#"+emphasizingNote.id).animate({left: left, top: top, width: width, height: height});
			

			top = this.get_client_height() - this.arrangeNotes.defaultActualMinHeight - this.arrangeNotes.verticalGap;
			width = this.arrangeNotes.defaultWidth;
			height = this.arrangeNotes.defaultMinHeight;

			for(var i = 1; i < notes.length; i++)
			{
				left = this.arrangeNotes.defaultLeft + (this.arrangeNotes.defaultActualWidth + this.arrangeNotes.horizontalGap) * (i - 1);
	
				var note = notes[i];
				note.left = left;
				note.top = top;
				note.width = width;
				note.height = height;

				// update UI
				$("#"+note.id).animate({left: left, top: top, width: width, height: height});
			}

			// save the position and size if have permission
			var permission = this.get_current_category_permission();
			if(permission === "owner" || permission === "rw")
			{
				this.arrange_notes(notes);
			}
		},

		get_sorted_notes: function(asc)
		{
			// get note array
			var notes=[];
			$notes = $(".note");
			for(var i = 0; i < $notes.length; i++)
			{
				var note = $notes[i];
				var $note = $(note);
				if($note.is(':visible'))
				{
					notes.push({id: note.id, z_index: parseInt($note.css("z-index"))});
				}
			}

			// sort note array
			if(asc)
			{
				return this.sort_notes_by_z_index_desc(notes);
			}
			else
			{
				return this.sort_notes_by_z_index_asc(notes);
			}
		},

		sort_notes_by_z_index_asc: function(notes)
		{
			return notes.sort(function(a, b){
				return a.z_index - b.z_index;
			});
		},

		sort_notes_by_z_index_desc: function(notes)
		{
			return notes.sort(function(a, b){
				return b.z_index - a.z_index;
			});
		},

		get_list_notes_left: function(index)
		{
			var result = 0;
			var noteCountPerRow = this.list_notes_count_per_row();
			var currentColumIndex = 0;
			
			for(var i = 0; i <= index; i++)
			{
				currentColumIndex = i % noteCountPerRow;

				if(currentColumIndex === 0)
				{
					result = this.arrangeNotes.defaultLeft;
				}
				else
				{
					result += this.arrangeNotes.defaultActualWidth + this.arrangeNotes.horizontalGap;
				}
			}

			return result;
		},

		get_list_small_notes_left: function(index)
		{
			var result = 0;
			var noteCountPerRow = this.list_small_notes_count_per_row();
			var currentColumIndex = 0;
			
			for(var i = 0; i <= index; i++)
			{
				currentColumIndex = i % noteCountPerRow;

				if(currentColumIndex === 0)
				{
					result = this.arrangeNotes.defaultLeft;
				}
				else
				{
					result += this.arrangeNotes.defaultActualMinWidth + this.arrangeNotes.horizontalGap;
				}
			}

			return result;
		},

		get_list_notes_top: function(index)
		{
			var result = 0;
			var noteCountPerRow = this.list_notes_count_per_row();
			
			for(var i = 0; i <= index; i++)
			{
				if(i === 0)
				{
					result = this.arrangeNotes.defaultTop;
				}
				else
				{
					if(i % noteCountPerRow === 0)
					{
						result += this.arrangeNotes.verticalGap + this.arrangeNotes.defaultActualHeight;
					}
				}
			}

			return result;
		},

		get_list_small_notes_top: function(index)
		{
			var result = 0;
			var noteCountPerRow = this.list_small_notes_count_per_row();
			
			for(var i = 0; i <= index; i++)
			{
				if(i === 0)
				{
					result = this.arrangeNotes.defaultTop;
				}
				else
				{
					if(i % noteCountPerRow === 0)
					{
						result += this.arrangeNotes.verticalGap + this.arrangeNotes.defaultActualMinHeight;
					}
				}
			}

			return result;
		},


		list_notes_count_per_row: function()
		{
			var result = 1;
			var clientWidth = this.get_client_width();
			var width = this.arrangeNotes.defaultLeft + this.arrangeNotes.defaultActualWidth;

			while(width + this.arrangeNotes.horizontalGap + this.arrangeNotes.defaultActualWidth <= clientWidth)
			{
				width += this.arrangeNotes.horizontalGap + this.arrangeNotes.defaultActualWidth;
				result ++;
			};

			return result;
		},


		list_small_notes_count_per_row: function()
		{
			var result = 1;
			var clientWidth = this.get_client_width();
			var width = this.arrangeNotes.defaultLeft + this.arrangeNotes.defaultActualMinWidth;

			while(width + this.arrangeNotes.horizontalGap + this.arrangeNotes.defaultActualMinWidth <= clientWidth)
			{
				width += this.arrangeNotes.horizontalGap + this.arrangeNotes.defaultActualMinWidth;
				result ++;
			};

			return result;
		},

		get_client_width: function()
		{
			var result = window.screen.width;

 			if (document.documentElement)
 			{
				result = document.documentElement.clientWidth;
			}
			else if (document.body)
			{
				result = document.body.clientWidth;
			}

			return result;
		},

		get_client_height: function()
		{
			var result = window.screen.height;

 			if (document.documentElement)
 			{
				result = document.documentElement.clientHeight;
			}
			else if (document.body)
			{
				result = document.body.clientHeight;
			}

			return result;
		},

		show_note: function(data)
		{
			if(data.z_index > this.z_index) this.z_index = data.z_index;
			
			$("#noteTemplate").tmpl(data).appendTo($("#notes"));
		},

		set_error_msg: function(msg, pingTheMsg)
		{
			if(msg != null && msg.length > 0)
			{
				window.clearTimeout(this.errorMsgTimeout);

				$("#errormsg").html(msg).fadeIn(1000);

				if(!pingTheMsg)
				{
					this.errorMsgTimeout = window.setTimeout(function(){$("#errormsg").fadeOut(2000);}, this.errorMsgTimeoutVal);
				}
			}
		},

		show_writing_icon: function(noteId)
		{
			var $writingIcon = $("#"+noteId).find(".writing_icon");
			$writingIcon.fadeIn();
		},

		hide_writing_icon: function(noteId)
		{
			var $writingIcon = $("#"+noteId).find(".writing_icon");
			$writingIcon.fadeOut(1000);
		},

		get_color: function($element)
		{
			var color = 6;
			
			if($element.hasClass("color1"))
			{
				color = 1;
			}
			else if($element.hasClass("color2"))
			{
				color = 2;
			}
			else if($element.hasClass("color3"))
			{
				color = 3;
			}
			else if($element.hasClass("color4"))
			{
				color = 4;
			}
			else if($element.hasClass("color5"))
			{
				color = 5;
			}
			else if($element.hasClass("color6"))
			{
				color = 6;
			}
			
			return color;
		},

		bind_events: function()
		{
			var me = this;

			$(".category_button").unbind().click(function(event)
			{
				var $note = $(event.target).parent().parent().parent();
				var noteId = $note.attr("id");
				me.currentNoteId = noteId;

				me.open_note_category_dialog(event.clientX, event.clientY);
			});
			

			$(".maximize_button").unbind().click(function(event)
			{
				var $note = $(event.target).parent().parent().parent();
				var noteId = $note.attr("id");

				var left = me.arrangeNotes.defaultLeft;
				var top = me.arrangeNotes.defaultTop;
				var width = me.get_client_width() - left - 20 - me.arrangeNotes.horizontalGap; // 20 is the note horizontal margin+border+padding
				var height = me.get_client_height() - me.arrangeNotes.defaultTop - 33 - me.arrangeNotes.verticalGap; // 33 is the note vertical margin+border+padding

				// update UI
				$("#"+noteId).animate({left: left, top: top, width: width, height: height});

				// only save if has permission
				var permission = me.get_current_category_permission();
				if(permission === "owner" || permission === "rw")
				{
					me.update_note_position(noteId, left, top);
					me.update_note_size(noteId, width, height);
				}
			});
			

			$(".minimize_button").unbind().click(function(event)
			{
				var $note = $(event.target).parent().parent().parent();
				var noteId = $note.attr("id");

				var left = me.arrangeNotes.defaultLeft;
				var top = me.arrangeNotes.defaultTop;
				var width = me.arrangeNotes.defaultMinWidth;
				var height = me.arrangeNotes.defaultMinHeight;

				// update UI
				$("#"+noteId).animate({left: left, top: top, width: width, height: height});

				// only save if has permission
				var permission = me.get_current_category_permission();
				if(permission === "owner" || permission === "rw")
				{
					me.update_note_position(noteId, left, top);
					me.update_note_size(noteId, width, height);
				}
			});
			

			$(".color_button").unbind().click(function(event)
			{
				var $color = $(event.target);
				var $note = $color.parent().parent().parent();
				
				var oldColor = me.get_color($note);
				var newColor = me.get_color($color);
				
				if(oldColor !== newColor)
				{
					// update color locally
					var newColorClass = "color" + newColor;
					var oldColorClass = "color" + oldColor;
					var newColorTitleClass = "colortitle" + newColor;
					var oldColorTitleClass = "colortitle" + oldColor;

					$note.removeClass(oldColorClass).addClass(newColorClass);
					$note.find(".header").removeClass(oldColorTitleClass).addClass(newColorTitleClass);
					$note.find(".body").removeClass(oldColorClass).addClass(newColorClass);

					var noteId = $note.attr("id");
					me.update_note_color(noteId, newColor);
				}
			});
			

			$(".delete_button").unbind().click(function(event)
			{
				if(window.confirm("Confirm deleting the note?"))
				{
					var $note = $(event.target).parent().parent().parent();
					var noteId = $note.attr("id");
					me.delete_note(noteId);
				}
			});
			

			$(".history_button").unbind().click(function(event)
			{
				var $note = $(event.target).parent().parent().parent();
				var noteId = $note.attr("id");
				me.open_note_history_dialog(noteId);
			});


			$(".body").unbind().focus(function(event) 
			{		
				var $note = $(event.target).parent();

				// bring the note to front
				$note.css('z-index', ++me.z_index);
		
				var permission = me.get_current_category_permission();

				// only owner can change note category
				if(permission === "owner")
				{
					$note.find(".category_button").show();
				}

				// only have "owner" or "rw" permission can see note buttons and update z_index
				if(permission === "owner" || permission === "rw")
				{

					$note.find(".color_button").show();
					$note.find(".delete_button").show();
					$note.find(".history_button").show();

					// update z_inex
					var noteId = $note.attr("id");
					me.update_note_z_index(noteId, me.z_index);
				}

				// always show minimize and maximize buttons
				$note.find(".minimize_button").show();
				$note.find(".maximize_button").show();

			}).blur(function(event)
			{
				$(event.target).parent().find(".note_button").hide();
			}).change(function(event)
			{
				window.clearTimeout(me.autoSavingTimeout);

				var $note = $(event.target).parent();
				var noteId = $note.attr("id");

				me.update_note_content(noteId);
			}).keyup(function(event)
			{
				var permission = me.get_current_category_permission();
				if(permission === "owner" || permission === "rw")
				{
					window.clearTimeout(me.autoSavingTimeout);

					var $note = $(event.target).parent();
					var noteId = $note.attr("id");

					me.show_writing_icon(noteId);

					me.autoSavingTimeout = window.setTimeout(function()
						{
							me.update_note_content(noteId);
						}, me.autoSavingTimeoutVal);
				}
			});


			$(".note").unbind('click').click(function(event)
			{
				// bring the note to front
				var $element = $(event.target);
				if($element.hasClass('header') 
					|| $element.hasClass('ui-resizable-handle') 
					|| $element.hasClass('note'))
				{
					var $note = null;
					if($element.hasClass('note'))
					{
						$note = $element
					}
					else
					{
						$note = $element.parent();
					}

					$note.css('z-index', ++me.z_index);
		
					var permission = me.get_current_category_permission();
					if(permission === "owner" || permission === "rw")
					{
						var noteId = $note.attr("id");
						me.update_note_z_index(noteId, me.z_index);
					}
				}	
			}).draggable({
				start: function(event, ui) 
				{
					ui.helper.css('z-index', ++me.z_index);
				},
				stop: function(event, ui) 
				{
					var permission = me.get_current_category_permission();
					if(permission === "owner" || permission === "rw")
					{
						var noteId = ui.helper.attr("id");
						me.update_note_position(noteId, ui.offset.left, ui.offset.top);
					}

				}
			}).resizable({
				minHeight: 18,
				minWidth: 190,
				start: function(event, ui) 
				{
					ui.helper.css('z-index', ++me.z_index);
				},
				stop: function(event, ui) 
				{
					var permission = me.get_current_category_permission();
					if(permission === "owner" || permission === "rw")
					{
						var noteId = ui.helper.attr("id");
						me.update_note_size(noteId, ui.size.width, ui.size.height);
					}
				}
			});
		}
	});

	tp.Note = Note;
	
})(jQuery, tp);
















$(document).ready(function() 
{
	tp.note = new tp.Note();
	tp.note.init();
});









