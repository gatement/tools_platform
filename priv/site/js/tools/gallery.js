if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Gallery()
	{
		var me = this;

		this.currentAlbumId = "";
		this.currentAlbumPermission = "owner";

		this.selectTreeviewNodeId = null;

		this.itemHeight = 110;
		this.errorMsgTimeout = {};
		this.errorMsgTimeoutVal = 8000;
		this.currentUploadIndex = 0;
		this.uploadMaxRetryTimes = 3;
		this.uploadRetryTimes = 0;

		$("#logout").click(function(){me.logout()});
		$("#about").click(function(){me.about()});
		$("#homepage").click(function(){me.homepage()});
		$("#addAlbum").click(function(){me.add_album()});
		$("#refresh").click(function(){me.load_items()});
		$("#upload").click(function(){me.upload_items()});
		$("#selectable").click(function(){me.selectable_click()});
		$("#unselectable").click(function(){me.unselectable_click()});
		$("#zoomin").click(function(){me.zoomin_click()});
		$("#zoomout").click(function(){me.zoomout_click()});
		$("#delete").click(function(){me.delete_click()});
		$("#rename").click(function(){me.rename_click()});
		$("#setAsCover").click(function(){me.setAsCover_click()});
		$("#move").click(function(){me.move_click()});
		$("#shareMgmt").click(function(){me.shareMgmt_click()});
		$("#back").click(function(){me.back_click()});		

		$("#filesToUpload").change(function(){me.filesSelected()});
		$("#startUpload").click(function(){me.startUpload()});
		$("#shareMgmtAddButton").click(function(){me.add_album_share()});
		$("#moveItems").click(function(){me.move_items_click()});
		$("#copyItems").click(function(){me.copy_items_click()});
	};

	$.extend(Gallery.prototype, {
		init: function()
		{
			this.load_items();
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

		add_album: function()
		{
			var me = this;

			var albumName = $.trim(window.prompt("Please input the album name", ""));
			var parentId = this.currentAlbumId;
				
			if(albumName)
			{
				var url = "/gallery/album/add";

				var data = {album_name: albumName, parent_id: parentId};

				var successFunc = function(response)
				{
					if(response.success)
					{
						me.load_items();
					}
					else
					{
						window.alert(response.data);
					}
				}

				var errorFunc = function()
				{
					window.alert("Creating album failed!");
				}

				this.ajax(url, data, successFunc, errorFunc);
			}
		},

		upload_items: function()
		{
			var me = this;

			// empty old upload item list
		    $("#uploadItemsContainer").empty();

		    // disable upload button
			$("#startUpload").attr("disabled","disabled");

			$("#fileUploaderDialog").dialog({modal: true, width: 620, minWidth: 620, close: function(event, ui) {me.close_upload_dialog();}});			
		},

		close_upload_dialog: function()
		{
			this.load_items();
		},

		selectable_click: function()
		{
			this.cancel_selectable();
		},

		unselectable_click: function()
		{
			var me = this;

			$("#unselectable").hide();
			$("#selectable").show();
			$("#galleryContainer").selectable({
				selected: function(event, ui) {window.setTimeout(function(){me.on_item_selected();}, 50)}
			});
		},

		zoomin_click: function()
		{
			this.itemHeight = window.parseInt(this.itemHeight * 1.2);
			this.load_items();
		},

		zoomout_click: function()
		{
			this.itemHeight = window.parseInt(this.itemHeight * 0.8);
			this.load_items();
		},

		delete_click: function()
		{
			if(window.confirm("Confirm deleting selected items?"))
			{
				var me = this;

				var itemIds = "";

				$(".ui-selected").each(function(index, element)
				{
					var itemId = $(element).attr("data-item-id");
					if(itemId != undefined)
					{
						itemIds += itemId + ",";
					}
				});

				if(itemIds.length > 0)
				{
					var url = "/gallery/item/delete";

					itemIds = itemIds.substr(itemIds, itemIds.length - 1);
					var data = {item_ids: itemIds};

					var successFunc = function(response)
					{
						var itemIdArray = itemIds.split(",");

						for(var i = 0; i < itemIdArray.length; i++)
						{
							var itemId = itemIdArray[i];
							if(response.failed_ids.indexOf(itemId) == -1)
							{
								$(".ui-selected[data-item-id="+itemId+"]").remove();
							}
						}

						if(response.failed_ids.length > 0)
						{
							window.alert("Some of the albums could not be deleted because of they are not empty or you do not have permission!");
						}

						me.on_item_selected();
					}

					var errorFunc = function()
					{
						window.alert("Deletion failed!");
					}

					this.ajax(url, data, successFunc, errorFunc);
				}
			}
		},

		rename_click: function()
		{
			var me = this;

			var itemId = $(".ui-selected").attr("data-item-id");

			var oldName = "";
			if($(".ui-selected").hasClass("album"))
			{
				oldName = $(".ui-selected span").html();
			}
			else
			{
				oldName = $(".ui-selected img").attr("title");
			}

			var newName = $.trim(window.prompt("Please input a new name", oldName));

			if(newName)
			{
				var url = "/gallery/item/rename";

				var data = {item_id: itemId, name: newName};

				var successFunc = function(response)
				{
					if(response.success)
					{
						if($(".ui-selected").hasClass("album"))
						{
							$(".ui-selected span").html(newName);
						}
						else
						{
							$(".ui-selected img").attr("title", newName);
						}
					}
					else
					{
						window.alert("You do not have permission to do rename!");						
					}
				}

				var errorFunc = function()
				{
					window.alert("Rename failed!");
				}

				this.ajax(url, data, successFunc, errorFunc);
			}
		},

		setAsCover_click: function()
		{
			var me = this;

			var itemId = $(".ui-selected").attr("data-item-id");			
			var url = "/gallery/item/setAsCover";
			var data = {item_id: itemId};

			var successFunc = function(response)
			{
			}

			var errorFunc = function()
			{
				window.alert("Failed to set as cover!");
			}

			this.ajax(url, data, successFunc, errorFunc);
		},

		move_click: function()
		{
			var me = this;
			this.selectTreeviewNodeId = null;
			$("#moveItems").attr("disabled", "disabled");
			$("#copyItems").attr("disabled", "disabled");

			$("#albumTreeviewContrainer").jstree({
				"plugins": ["themes","json_data","ui","types"],
				"json_data": {
		            "ajax": {
		                "url": "/gallery/album/treeview/children",
		                "data": function (node) {
		                    return {
		                        "parent_id": node.attr ? node.attr("id").replace("node_","") : ""
		                    };
		                }
		            }
		        }
			})
			.bind("select_node.jstree", function (event, data) {
            	me.selectTreeviewNodeId = data.rslt.obj.attr("id");
            	$("#moveItems").removeAttr("disabled");
            	$("#copyItems").removeAttr("disabled");
        	});

			$("#moveItemsDialog").dialog({modal: true, width: 620, minWidth: 620, close: function(event, ui) {me.close_moveItem_dialog();}});			
		},

		close_moveItem_dialog: function()
		{
			$("#albumTreeviewContrainer").jstree("destroy");
		},

		move_items_click: function()
		{
			var me = this;

			var itemIds = "";
			$(".ui-selected").each(function(index, element)
			{
				var itemId = $(element).attr("data-item-id");
				if(itemId != undefined)
				{
					itemIds += itemId + ",";
				}
			});			

			if(itemIds.length > 0 && this.selectTreeviewNodeId != null)
			{
				var url = "/gallery/item/move";

				itemIds = itemIds.substr(itemIds, itemIds.length - 1);
				var targetItemId = this.selectTreeviewNodeId;
				var data = {item_ids: itemIds, target_item_id: targetItemId};

				var successFunc = function(response)
				{
					var itemIdArray = itemIds.split(",");

					for(var i = 0; i < itemIdArray.length; i++)
					{
						var itemId = itemIdArray[i];
						if(response.failed_ids.indexOf(itemId) == -1)
						{
							$(".ui-selected[data-item-id="+itemId+"]").remove();
						}
					}

					if(response.failed_ids.length > 0)
					{
						window.alert("Some of the items could not be moved because you have no permission or they are already in the target album or you can move it into itself or the last root album can not be moved!");
					}
					else
					{
						$("#moveItemsDialog").dialog("close");
					}

					me.on_item_selected();
				}

				var errorFunc = function()
				{
					window.alert("Movement failed!");
				}

				this.ajax(url, data, successFunc, errorFunc);
			}
		},

		copy_items_click: function()
		{
			var me = this;

			var itemIds = "";
			$(".ui-selected").each(function(index, element)
			{
				if(!$(element).hasClass("album"))
				{
					var itemId = $(element).attr("data-item-id");
					if(itemId != undefined)
					{
						itemIds += itemId + ",";
					}
				}
			});

			if(itemIds.length > 0 && this.selectTreeviewNodeId != null)
			{
				var url = "/gallery/item/copy";

				itemIds = itemIds.substr(itemIds, itemIds.length - 1);
				var targetItemId = this.selectTreeviewNodeId;
				var data = {item_ids: itemIds, target_item_id: targetItemId};

				var successFunc = function(response)
				{
					if(response.failed_ids.length > 0)
					{
						window.alert("Some of the items could not be copied because you have no permission or they are already in the target album!");
					}
					else
					{
						$("#moveItemsDialog").dialog("close");
					}

					me.on_item_selected();
				}

				var errorFunc = function()
				{
					window.alert("Copy failed!");
				}

				this.ajax(url, data, successFunc, errorFunc);
			}
			else
			{
				window.alert("Sorry, only non-album could be copied!");
			}
		},

		shareMgmt_click: function()
		{
			this.load_album_shares();
			$("#shareMgmtDialog").dialog({modal: true, width: 500, minWidth: 500});
			$("#shareMgmtAddTextbox").focus();
		},

		back_click: function()
		{
			var me = this;

			var url = "/gallery/item/info/" + this.currentAlbumId;

			var data = {include_self: false};

			var successFunc = function(response)
			{
				me.currentAlbumId = response.data.parent_id;
				$("#ancestorsPath").text(response.data.ancestor_path);
				me.currentAlbumPermission = response.data.permission;
				me.load_items();
			}

			var errorFunc = function()
			{
				window.alert("Operation failed!");
			}

			this.ajax(url, data, successFunc, errorFunc);			
		},

		//============ IO ==========================================================================

		ajax: function(url, data, successFunc, errorFunc, method) 
		{
			data = data || {};
			method = method ? method : "POST";

			$.ajax({
				    url: url,
				    type: method,
				    data: data,
				    success: function (response, textStatus, jqXHR) 
		            {
		            	if(successFunc) successFunc(response);
				    },
				    error: function (jqXHR, textStatus, errorThrown)
		            {		        
				        if(errorFunc) errorFunc();
				    }
				});
		},


		//============ upload =======================================================================

		filesSelected: function() 
		{
			$("#startUpload").removeAttr("disabled");
		    var $uploadItemsContainer = $("#uploadItemsContainer").empty();
		    var filesToUpload = document.getElementById('filesToUpload');

			for(var i = 0; i < filesToUpload.files.length; i++)
			{
			    var file = filesToUpload.files[i];

		        var fileSize = 0;
		        if (file.size > 1024 * 1024)
		        {
		            fileSize = (Math.round(file.size * 100 / (1024 * 1024)) / 100).toString() + 'MB';
		        }
		        else
		        {
		            fileSize = (Math.round(file.size * 100 / 1024) / 100).toString() + 'KB';
		        }

		        var itemData = {id: i, name: file.name, size: fileSize};

				$("#uploadItemTemplate").tmpl(itemData).appendTo($uploadItemsContainer);
			}
		},

		startUpload: function()
		{
			// disable upload button
			$("#startUpload").attr("disabled","disabled");

			if(this.currentUploadIndex < document.getElementById('filesToUpload').files.length)
			{
				this.uploadFile();
			}
			else
			{
				this.endUpload();
			}
		},

		endUpload: function()
		{
			this.uploadRetryTimes = 0;
			this.currentUploadIndex = 0;

			if($(".uploadItem .uploadError").size() === 0)
			{
				$("#fileUploaderDialog").dialog("close");
			}
		},

		uploadFile: function()
		{
			var me = this;

		    var fd = new FormData();
		    fd.append("fileToUpload", document.getElementById('filesToUpload').files[this.currentUploadIndex]);

		    var xhr = new XMLHttpRequest();
		    xhr.upload.addEventListener("progress", function(evt){me.uploadProgress(evt)}, false);
		    xhr.addEventListener("load", function(evt){me.uploadComplete(evt)}, false);
		    xhr.addEventListener("error", function(evt){me.uploadFailed(evt)}, false);
		    xhr.addEventListener("abort", function(evt){me.uploadCanceled(evt)}, false);
		    xhr.open("POST", "/gallery/item/upload/" + this.currentAlbumId);
		    xhr.send(fd);
		},

		uploadProgress: function(evt)
		{
			var percentComplete = 0;

		    if (evt.lengthComputable)
		    {
		        percentComplete = Math.round(evt.loaded * 100 / evt.total);
		    }

		    $("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemProgress").css("width", percentComplete.toString() + "%");
		    $("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemUploadPercentage").html(percentComplete);
		},

		uploadComplete: function(evt)
		{
 			$("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemProgress").css("width", "100%");
		    $("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemUploadPercentage").html(100);
		    $("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemProgress").addClass("uploadComplete");
			this.uploadRetryTimes = 0;
			this.currentUploadIndex ++;
		    this.startUpload();	
		},

		uploadFailed: function(evt)
		{
			if(this.uploadRetryTimes < this.uploadMaxRetryTimes - 1)
			{
				// retry
				this.uploadRetryTimes ++;
				this.uploadFile();
			}
			else
			{
				// give up and upload next file
		    	$("#uploadItem" + this.currentUploadIndex.toString() + " .uploadItemProgress").addClass("uploadError");

				this.uploadRetryTimes = 0;
				this.currentUploadIndex ++;
		    	this.startUpload();	
			}
		},

		uploadCanceled: function(evt)
		{
			if(console.log)
			{
		    	console.log("The upload has been canceled by the user or the browser dropped the connection.");
		    }
		},

		//============ Helpers ======================================================================

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

		load_items: function()
		{
			var me = this;

			this.cancel_selectable();
			this.show_hide_toolbar_buttons();

			var $galleryContainer = $("#galleryContainer");
			$galleryContainer.empty();

			$("#infiniteLoading").show();

			var url = "/gallery/item/list";

			var data = {parent_id: this.currentAlbumId, height: this.itemHeight};

			var successFunc = function(result)
			{
				if(result.success)
				{
					var lastItemType = "";
					for(var i = 0; i < result.data.length; i++)
					{
						item = result.data[i];

						// seperate albums and normal items with a <br/>
						if(lastItemType === "album" && item.type !== "album")
						{
							$galleryContainer.append('<br style="clear:both;"/>');
						}
						lastItemType = item.type;

						switch(item.type)
						{
							case "album":
								$("#albumItemTemplate").tmpl(item).appendTo($galleryContainer);
								break;
							case "image":
								$("#imageItemTemplate").tmpl(item).appendTo($galleryContainer);
								break;
							case "video":
								$("#videoItemTemplate").tmpl(item).appendTo($galleryContainer);
								break;
						}
					}

					$(".album").unbind().click(function(event)
					{
						me.album_click(event);
					});

					$(".image").unbind().click(function(event)
					{
						me.image_click(event);
					});

					$(".video").unbind().click(function(event)
					{
						me.video_click(event);
					});

					$(".galleryItem img").load(function(event){
						$(event.target).removeAttr("width");
					});
				}

				$("#infiniteLoading").hide();
			}

			var errorFunc = function()
			{
				$("#infiniteLoading").hide();
				window.alert("Load items failed!");
			}

			this.ajax(url, data, successFunc, errorFunc);
		},

		album_click: function(event)
		{
			var me = this;

			if($("#unselectable").is(":visible") || (!$("#selectable").is(":visible") && !$("#unselectable").is(":visible")))
			{
				this.currentAlbumId = $(event.target).parent().attr("data-item-id");

				var url = "/gallery/item/info/" + this.currentAlbumId;
				var data = {include_self: true};

				var successFunc = function(response)
				{
					me.currentAlbumPermission = response.data.permission;
					$("#ancestorsPath").text(response.data.ancestor_path);
					me.load_items();
				}

				var errorFunc = function()
				{
					window.alert("Operation failed!");
				}

				this.ajax(url, data, successFunc, errorFunc);	
			}
		},

		image_click: function(event)
		{
			if($("#unselectable").is(":visible") || (!$("#selectable").is(":visible") && !$("#unselectable").is(":visible")))
			{
				var $image = $(event.target);
				var itemId = $image.parent().attr("data-item-id");
				var imageUrl = $image.attr('data-url');
				var windowName = itemId;
				window.open(imageUrl, windowName, "toolbar=no,menubar=no,scrollbars=yes,resizable=yes,location=no,status=no");
			}
		},

		video_click: function(event)
		{
			if($("#unselectable").is(":visible"))
			{
				window.alert("image_click");
			}
		},

		load_album_shares: function() 
		{
			var me = this;

			$("#shareMgmtList").empty();

			var url = "/gallery/share/list";
			var data = {item_id: this.currentAlbumId};

			var successFunc = function(response) 
			{
				if(response.success)
				{
					var $shareMgmtList = $("#shareMgmtList");

					for(var i = 0; i < response.data.length; i++)
					{
						$("#shareMgmtListItemTemplate").tmpl(response.data[i]).appendTo($shareMgmtList);
					}

					$(".shareMgmtListItemDeleteButton").click(function(event){me.delete_album_share(event)});
				}
				else
				{
					window.alert(response.data);
				}
			};

			var errorFunc = function() 
			{
				window.alert("Load share list failed!");
			};

			this.ajax(url, data, successFunc, errorFunc);
		},

		delete_album_share: function(event) 
		{
			var me = this;

			var url = "/gallery/share/delete";

			var shareId = $(event.target).parent().find(".shareMgmtListItemTextbox").attr("data-share-id");
			var data = {share_id: shareId};

			var successFunc = function(response) 
			{
				if(response.success)
				{
					me.load_album_shares();
				}
				else
				{
					window.alert(response.data);
				}				
			};

			var errorFunc = function(data) 
			{
				window.alert("Delete album share failed!");
			};

			this.ajax(url, data, successFunc, errorFunc);
		},

		show_hide_toolbar_buttons: function()
		{
			// it is in root
			if(this.currentAlbumId === "")
			{
				$("#upload").hide();
				$("#shareMgmt").hide();
				$("#back").hide();
				$("#unselectable").show();
			}
			else
			{
				// my album
				if(this.currentAlbumPermission === "owner")
				{
					$("#upload").show();
					$("#shareMgmt").show();	
					$("#unselectable").show();
				}
				// in shared album
				else
				{
					$("#addAlbum").hide();
					$("#upload").hide();
					$("#shareMgmt").hide();
					$("#unselectable").hide();
				}

				$("#back").show();
			}
		},

		on_item_selected: function()
		{
			var selectedCount = $(".ui-selected").size();
			//console.log("size: %o, image: %o, albumId: %o", selectedCount, $(".ui-selected").hasClass("image"), this.currentAlbumId);
			if(selectedCount == 1)
			{
				$("#delete").show();
				$("#rename").show();
				$("#move").show();
				if(this.currentAlbumId !== "" && $(".ui-selected").hasClass("image"))
				{
					$("#setAsCover").show();
				}
				else
				{
					$("#setAsCover").hide();
				}
			}
			else if(selectedCount > 1)
			{
				$("#delete").show();
				$("#move").show();
				$("#rename").hide();
				$("#setAsCover").hide();
			}
			else
			{
				$("#delete").hide();
				$("#rename").hide();
				$("#setAsCover").hide();
				$("#move").hide();
			}
		},

		cancel_selectable: function()
		{
			if($("#selectable").is(":visible"))
			{
				$("#selectable").hide();
				$("#unselectable").show();

				$("#delete").hide();
				$("#rename").hide();
				$("#setAsCover").hide();
				$("#move").hide();

				$("#galleryContainer").selectable("destroy");
				$(".galleryItem").removeClass("ui-selected");
			}
		},

		add_album_share: function() 
		{
			var me = this;

			var sharedUserId = $.trim($("#shareMgmtAddTextbox").val());
			var albumItemId = this.currentAlbumId;
			var permission = $("#shareMgmtAddSelect").val();

			if(sharedUserId)
			{
				var url = "/gallery/share/add";
			    var data = {shared_user_id: sharedUserId, item_id: albumItemId, permission: permission};

				var successFunc = function(response) 
				{
					if(response.success)
					{
						$("#shareMgmtAddTextbox").val("");
						me.load_album_shares();
					}
					else
					{
						window.alert(response.data);
					}
				};

				var errorFunc = function(response) 
				{
					window.alert(response.data);
				};

				this.ajax(url, data, successFunc, errorFunc);
		    }
		}
	});

	tp.Gallery = Gallery;
	
})(jQuery, tp);


$(document).ready(function() 
{
	tp.gallery = new tp.Gallery();
	tp.gallery.init();
});
