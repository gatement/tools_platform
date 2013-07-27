
function ajax(url, data, successFunc, errorFunc, method)
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
}

function load_pub_permissions()
{
	var url = "/mqtt/pub_permission/list";
	var data = {};

	var successFunc = function(result)
	{
		$pubPermissions = $("#pubPermissions");
		$pubPermissions.empty();

		for(var i = 0; i < result.data.length; i++)
		{
			$("#pubPermissionTemplate").tmpl(result.data[i]).appendTo($pubPermissions);
		}

		$(".saveBtn").unbind().click(function(event){
			save_pub_permission(event);
		});

		$(".deleteBtn").unbind().click(function(event){
			delete_pub_permission(event);
		});

		$(".checkClientBtn").unbind().click(function(event){
			check_client(event);
		});
	};
	
	var errorFunc = function()
	{
		window.alert("list error!");
	};

	ajax(url, data, successFunc, errorFunc);
}

function add_pub_permission()
{
	var clientId = $.trim($("#clientIdAddInput").val());
	var userId = $.trim($("#userIdAddInput").val());
	var topic = $.trim($("#topicAddInput").val());
	var desc = $.trim($("#descAddInput").val());
	var enabled = $("#enabledAddInput")[0].checked;
	if(clientId === "" || 
		topic === "" || 
		desc === "")
	{
		window.alert("clientId, topic and desc are required to create a permission.");
	}
	else
	{
		var data = {client_id: clientId, user_id: userId, topic: topic, desc: desc, enabled:enabled};

		var url = "/mqtt/pub_permission/add";

		var successFunc = function(result)
		{
			$("#clientIdAddInput").val("");
			$("#userIdAddInput").val("");
			$("#topicAddInput").val("");
			$("#descAddInput").val("");
			load_pub_permissions();
		};
		
		var errorFunc = function()
		{
			window.alert("adding error!");
		};

		ajax(url, data, successFunc, errorFunc);
	}
}

function save_pub_permission(event)
{
	var id = $(event.target).parent().parent().find(".id").val();
	var clientId = $.trim($(event.target).parent().parent().find(".clientId").val());
	var userId = $.trim($(event.target).parent().parent().find(".userId").val());
	var topic = $.trim($(event.target).parent().parent().find(".topic").val());
	var desc = $.trim($(event.target).parent().parent().find(".desc").val());
	var enabled = $(event.target).parent().parent().find(".enabled")[0].checked;
	if(clientId === "" || 
		topic === "" || 
		desc === "")
	{
		window.alert("clientId, topic and desc are required.");
	}
	else
	{
		var data = {id: id, client_id: clientId, user_id: userId, topic: topic, desc: desc, enabled: enabled};

		var url = "/mqtt/pub_permission/update";

		var successFunc = function(result)
		{
			load_pub_permissions();
		};
		
		var errorFunc = function()
		{
			window.alert("saving error!");
		};

		ajax(url, data, successFunc, errorFunc);
	}
}

function delete_pub_permission(event)
{
	if(window.confirm("Confirm deleting the permission?"))
	{
		var pubPermissionId = $(event.target).parent().parent().find(".id").val();
		var data = {pub_permission_id: pubPermissionId};

		var url = "/mqtt/pub_permission/delete";

		var successFunc = function(result)
		{
			load_pub_permissions();
		};
		
		var errorFunc = function()
		{
			window.alert("deleting error!");
		};

		ajax(url, data, successFunc, errorFunc);
	}
}

function search_by_client_id()
{
	var key = $("#clientIdSearchInput").val().toLowerCase();
	search_pub_permissions(key, ".clientId")
}

function search_by_user_id()
{
	var key = $("#userIdSearchInput").val().toLowerCase();
	search_pub_permissions(key, ".userId")
}

function search_by_topic()
{
	var key = $("#topicSearchInput").val().toLowerCase();
	search_pub_permissions(key, ".topic")
}

function search_by_desc()
{
	var key = $("#descSearchInput").val().toLowerCase();
	search_pub_permissions(key, ".desc")
}

function search_by_enabled()
{
	clear_search();

	var value = $("#enabledSearchInput")[0].checked;

	$subPermissions = $(".pubPermission");
	for(var i = 0; i < $subPermissions.length; i++)
	{
		var $subPermission = $($subPermissions[i]);
		if($subPermission.find(".enabled")[0].checked != value)
		{
			$subPermission.hide();
		}
		else
		{
			$subPermission.show();
		}
	}
}

function search_pub_permissions(key, field)
{
	$subPermissions = $(".pubPermission:visible");
	for(var i = 0; i < $subPermissions.length; i++)
	{
		var $subPermission = $($subPermissions[i]);
		if($subPermission.find(field).val().toLowerCase().indexOf(key) == -1)
		{
			$subPermission.hide();
		}
		else
		{
			$subPermission.show();
		}
	}
}

function clear_search()
{
	$("#clientIdSearchInput").val("");
	$("#userIdSearchInput").val("");
	$("#topicSearchInput").val("");
	$("#descSearchInput").val("");
	$(".pubPermission").show();
}

function check_client(event)
{
	var clientId = $(event.target).parent().parent().find(".clientId").text();
	var data = {client_id: clientId};

	var url = "/mqtt/client/is_online";

	var successFunc = function(result)
	{
		if(result.data)
		{
			$(event.target).val("online");
		}
		else
		{
			$(event.target).val("offline");
		}
	};
	
	var errorFunc = function()
	{
		window.alert("deleting error!");
	};

	ajax(url, data, successFunc, errorFunc);
}

$(document).ready(function() {
	$("#clientIdSearchInput").keyup(function(){
		search_by_client_id();
	});
	$("#userIdSearchInput").keyup(function(){
		search_by_user_id();
	});
	$("#topicSearchInput").keyup(function(){
		search_by_topic();
	});
	$("#descSearchInput").keyup(function(){
		search_by_desc();
	});
	$("#enabledSearchInput").change(function(){
		search_by_enabled();
	});
	$("#clearSerachBtn").click(function(){
		clear_search();
	});
	$("#addBtn").click(function(){
		add_pub_permission();
	});

	$(document).keydown(function(event) 
	{ 
		if(event.keyCode == 13) 
		{ 
			add_pub_permission();
		}
		else if(event.keyCode == 27)
		{
			clear_search();
		}
	});

	load_pub_permissions();
});
