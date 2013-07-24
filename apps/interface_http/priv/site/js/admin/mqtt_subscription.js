
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

function load_subscriptions()
{
	var url = "/mqtt/subscription/list";
	var data = {};

	var successFunc = function(result)
	{
		$subscriptions = $("#subscriptions");
		$subscriptions.empty();

		for(var i = 0; i < result.data.length; i++)
		{
			$("#subscriptionTemplate").tmpl(result.data[i]).appendTo($subscriptions);
		}

		$(".saveBtn").unbind().click(function(event){
			save_subscription(event);
		});

		$(".deleteBtn").unbind().click(function(event){
			delete_subscription(event);
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

function save_subscription(event)
{
	var id = $(event.target).parent().parent().find(".id").val();
	var clientId = $.trim($(event.target).parent().parent().find(".clientId").val());
	var topic = $.trim($(event.target).parent().parent().find(".topic").val());
	var qos = $.trim($(event.target).parent().parent().find(".qos").val());
	var ttl = $.trim($(event.target).parent().parent().find(".ttl").val());
	var desc = $.trim($(event.target).parent().parent().find(".desc").val());
	if(clientId === "" || 
		topic === "" || 
		qos === "" ||
		ttl === "" || 
		desc === "")
	{
		window.alert("clientId, topic, qos, ttl and desc are required.");
	}
	else
	{
		var data = {id: id, client_id: clientId, topic: topic, qos: qos, ttl: ttl, desc: desc};

		var url = "/mqtt/subscription/update";

		var successFunc = function(result)
		{
			load_subscriptions();
		};
		
		var errorFunc = function()
		{
			window.alert("saving error!");
		};

		ajax(url, data, successFunc, errorFunc);
	}
}


function add_subscription()
{
	var clientId = $.trim($("#clientIdAddInput").val());
	var topic = $.trim($("#topicAddInput").val());
	var qos = $.trim($("#qosAddInput").val());
	var ttl = $.trim($("#ttlAddInput").val());
	var desc = $.trim($("#descAddInput").val());
	if(clientId === "" || 
		topic === "" || 
		qos === "" ||
		ttl === "" ||
		desc === "")
	{
		window.alert("can not leave any field be empty.");
	}
	else
	{
		var data = {client_id: clientId, topic: topic, qos: qos, ttl: ttl,  desc: desc};

		var url = "/mqtt/subscription/add";

		var successFunc = function(result)
		{
			$("#clientIdAddInput").val("");
			$("#topicAddInput").val("");
			$("#qosAddInput").val("");
			$("#ttlAddInput").val("");
			$("#descAddInput").val("");
			load_subscriptions();
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
	search_subscriptions(key, ".clientId")
}

function search_by_topic()
{
	var key = $("#topicSearchInput").val().toLowerCase();
	search_subscriptions(key, ".topic")
}

function search_by_qos()
{
	var key = $("#qosSearchInput").val().toLowerCase();
	search_subscriptions(key, ".qos")
}

function search_by_ttl()
{
	var key = $("#ttlSearchInput").val().toLowerCase();
	search_subscriptions(key, ".ttl")
}

function search_by_desc()
{
	var key = $("#descSearchInput").val().toLowerCase();
	search_subscriptions(key, ".desc")
}

function search_subscriptions(key, field)
{
	$subscriptions = $(".subscription:visible");
	for(var i = 0; i < $subscriptions.length; i++)
	{
		var $subscription = $($subscriptions[i]);
		if($subscription.find(field).val().toLowerCase().indexOf(key) == -1)
		{
			$subscription.hide();
		}
		else
		{
			$subscription.show();
		}
	}
}

function clear_search()
{
	$("#clientIdSearchInput").val("");
	$("#topicSearchInput").val("");
	$("#qosSearchInput").val("");
	$("#ttlSearchInput").val("");
	$("#descSearchInput").val("");
	$(".subscription").show();
}

function delete_subscription(event)
{
	if(window.confirm("Confirm deleting the subscription?"))
	{
		var subscriptionId = $(event.target).parent().parent().find(".id").val();
		var data = {subscription_id: subscriptionId};

		var url = "/mqtt/subscription/delete";

		var successFunc = function(result)
		{
			load_subscriptions();
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
	$("#topicSearchInput").keyup(function(){
		search_by_topic();
	});
	$("#qosSearchInput").keyup(function(){
		search_by_qos();
	});
	$("#ttlSearchInput").keyup(function(){
		search_by_ttl();
	});
	$("#descSearchInput").keyup(function(){
		search_by_desc();
	});
	$("#clearSerachBtn").click(function(){
		clear_search();
	});
	$("#addBtn").click(function(){
		add_subscription();
	});

	$(document).keydown(function(event) 
	{ 
		if(event.keyCode == 13) 
		{ 
			add_subscription();
		}
		else if(event.keyCode == 27)
		{
			clear_search();
		}
	});

	load_subscriptions();
});
