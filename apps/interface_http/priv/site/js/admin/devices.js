
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

function load_devices()
{
	var url = "/device_mgmt/list";
	var data = {};

	var successFunc = function(result)
	{
		$devices = $("#devices");
		$devices.empty();

		for(var i = 0; i < result.data.length; i++)
		{
			$("#deviceTemplate").tmpl(result.data[i]).appendTo($devices);
		}

		$(".saveBtn").unbind().click(function(event){
			save_device(event);
		});

		$(".deleteBtn").unbind().click(function(event){
			delete_device(event);
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

function save_device(event)
{
	var deviceId = $(event.target).parent().parent().find(".deviceId").val();
	var name = $(event.target).parent().parent().find(".name").val();
	var userId = $(event.target).parent().parent().find(".userId").val();
	var type = $(event.target).parent().parent().find(".type").val();
	if(name === "" || userId === "")
	{
		window.alert("Name, User Id are required.");
	}
	else
	{
		var data = {device_id: deviceId, name: name, user_id: userId, type: type};

		var url = "/device_mgmt/update";

		var successFunc = function(result)
		{
			load_devices();
		};
		
		var errorFunc = function()
		{
			window.alert("saving error!");
		};

		ajax(url, data, successFunc, errorFunc);
	}
}

function search_by_device_id()
{
	var key = $("#deviceIdSearchInput").val().toLowerCase();
	search_devices(key, ".deviceId")
}

function search_by_name()
{
	var key = $("#nameSearchInput").val().toLowerCase();
	search_devices(key, ".name")
}

function search_by_user_id()
{
	var key = $("#userIdSearchInput").val().toLowerCase();
	search_devices(key, ".userId")
}

function search_devices(key, field)
{
	$devices = $(".device:visible");
	for(var i = 0; i < $devices.length; i++)
	{
		var $device = $($devices[i]);
		if($device.find(field).val().toLowerCase().indexOf(key) == -1)
		{
			$device.hide();
		}
		else
		{
			$device.show();
		}
	}
}

function clear_search()
{
	$("#deviceIdSearchInput").val("");
	$("#nameSearchInput").val("");
	$("#userIdSearchInput").val("");
	$(".device").show();
}

function delete_device(event)
{
	if(window.confirm("Confirm deleting the device?"))
	{
		var deviceId = $(event.target).parent().parent().find(".deviceId").val();
		var data = {device_id: deviceId};

		var url = "/device_mgmt/delete";

		var successFunc = function(result)
		{
			load_devices();
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
	var deviceId = $(event.target).parent().parent().find(".deviceId").val();
	var data = {client_id: deviceId};

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
	$("#deviceIdSearchInput").keyup(function(){
		search_by_device_id();
	});
	$("#nameSearchInput").keyup(function(){
		search_by_name();
	});
	$("#userIdSearchInput").keyup(function(){
		search_by_user_id();
	});
	$("#clearSerachBtn").click(function(){
		clear_search();
	});

	$(document).keydown(function(event) 
	{ 
		if(event.keyCode == 27)
		{
			clear_search();
		}
	});

	load_devices();
});
