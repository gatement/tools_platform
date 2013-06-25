
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
			$("#subscriptionTemplate").tmpl(result.data[i]).appendTo($pubPermissions);
		}

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
	if(clientId === "" || topic === "")
	{
		window.alert("Client Id, Topic, Qos are required to create a subscription.");
	}
	else
	{
		var desc = $.trim($("#descAddInput").val());
		var data = {client_id: clientId, user_id: userId, topic: topic, desc: desc};

		var url = "/mqtt/sub_permission/add";

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

function search_pub_permissions(key, field)
{
	$subPermission = $(".subPermission");
	for(var i = 0; i < $subPermission.length; i++)
	{
		var $subPermission = $($subPermission[i]);
		if($subPermission.find(field).text().toLowerCase().indexOf(key) == -1)
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
	$(".subPermission").show();
}

function delete_pub_permission(event)
{
	if(window.confirm("Confirm deleting the subscription?"))
	{
		var pubPermissionId = $(event.target).parent().parent().find(".pubPermissionId").val();
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
	$("#clearSerachBtn").click(function(){
		clear_search();
	});
	$("#addBtn").click(function(){
		add_pub_permission();
	});

	$("body").keypress(function(arg){
		if(arg.keyCode === 13)
		{
			add_pub_permission();
		}
		else if(arg.keyCode === 27)
		{
			clear_search();
		}

	});

	load_pub_permissions();
});