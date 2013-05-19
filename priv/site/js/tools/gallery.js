if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Gallery()
	{
		var me = this;

		this.currentAlbumId = null;
		this.itemHeight = 120;
		this.errorMsgTimeout = {};
		this.errorMsgTimeoutVal = 8000;
		this.currentUploadIndex = 0;
		this.uploadMaxRetryTimes = 3;
		this.uploadRetryTimes = 0;

		$("#logout").click(function(){me.logout()});
		$("#about").click(function(){me.about()});
		$("#homepage").click(function(){me.homepage()});
		$("#addAlbun").click(function(){me.add_album()});
		$("#refresh").click(function(){me.load_items()});
		$("#upload").click(function(){me.upload_items()});

		$("#filesToUpload").change(function(){me.filesSelected()});
		$("#startUpload").click(function(){me.startUpload()});
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
				
			if(albumName)
			{
				var url = "/gallery/album/add";

				var data = {album_name: albumName};

				var successFunc = function()
				{
					me.load_items();
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
			$("#fineuploaderDialog").dialog({modal: true, width: 620, minWidth: 620, close: function(event, ui) {me.finish_upload();}});			
		},

		finish_upload: function()
		{
			alert("finish_upload");
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


		//============ upload =======================================================================
		filesSelected: function() 
		{
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
		    xhr.open("POST", "/gallery/item/upload");
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

			var $galleryContainer = $("#galleryContainer");
			$galleryContainer.empty();

			this.show_hide_toolbar_buttons();

			var url = "/gallery/item/list";

			var data = {parent_id: this.currentAlbumId, height: this.itemHeight};

			var successFunc = function(result)
			{
				if(result.success)
				{
					for(var i = 0; i < result.data.length; i++)
					{
						item = result.data[i];
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
				}
			}

			var errorFunc = function()
			{
				window.alert("Load items failed!");
			}

			this.ajax(url, data, successFunc, errorFunc);
		},

		album_click: function(event)
		{
			this.currentAlbumId = $(event.target).attr("data-album-id");
			this.load_items();
		},

		image_click: function(event)
		{
			alert("image_click");
		},

		video_click: function(event)
		{
			alert("video_click");
		},

		show_hide_toolbar_buttons: function()
		{
			if(this.currentAlbumId === null)
			{
				$("#upload").hide();
				$("#shareMgmt").hide();	
				$("#back").hide();
			}
			else
			{
				$("#upload").show();
				$("#shareMgmt").show();	
				$("#back").show();
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
