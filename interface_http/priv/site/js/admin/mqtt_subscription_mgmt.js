
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

function add_subscription()
{
	var clientId = $.trim($("#clientAddInput").val());
	var topic = $.trim($("#topicAddInput").val());
	if(clientId === "" || topic === "")
	{
		window.alert("client id and topic are required to create a subscription.");
	}
	else
	{
		var data = {client_id: clientId, topic: topic};

		var url = "/mqtt/subscription/add";

		var successFunc = function(result)
		{
			$("#clientAddInput").val("");
			$("#topicAddInput").val("");
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
	var key = $("#clientSearchInput").val().toLowerCase();
	search_subscriptions(key, ".clientId")
}

function search_by_topic()
{
	var key = $("#topicSearchInput").val().toLowerCase();
	search_subscriptions(key, ".topic")
}

function search_subscriptions(key, field)
{
	$subscriptions = $(".subscription");
	for(var i = 0; i < $subscriptions.length; i++)
	{
		var $subscription = $($subscriptions[i]);
		if($subscription.find(field).text().toLowerCase().indexOf(key) == -1)
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
	$("#clientSearchInput").val("");
	$("#topicSearchInput").val("");
	$(".subscription").show();
}

function delete_subscription(event)
{
	if(window.confirm("Confirm deleting the subscription?"))
	{
		var subscriptionId = $(event.target).parent().parent().find(".subscriptionId").val();
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
	$("#clientSearchInput").keyup(function(){
		search_by_client_id();
	});
	$("#topicSearchInput").keyup(function(){
		search_by_topic();
	});
	$("#clearSerachBtn").click(function(){
		clear_search();
	});
	$("#addBtn").click(function(){
		add_subscription();
	});

	$("body").keypress(function(arg){
		if(arg.keyCode === 13)
		{
			add_subscription();
		}
		else if(arg.keyCode === 27)
		{
			clear_search();
		}

	});

	load_subscriptions();
});