
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

function searchUsers()
{
	var val = $.trim($("#searchVal").val());
	var data = {term: val};

	var url = "/user/search";

	var successFunc = function(result)
	{
		$users = $("#users");
		$usersTable = $("#usersTable");

		if(result.length == 0)
		{
			$usersTable.hide();
		}
		else
		{
			$usersTable.show();
			$users.empty();

			for(var i = 0; i < result.length; i++)
			{
				$("#userTemplate").tmpl(result[i]).appendTo($users);
			}
		}
	};
	
	var errorFunc = function()
	{
		window.alert("search error!");
	}

	ajax(url, data, successFunc, errorFunc);
}

$(document).ready(function() {
	$("#searchVal").focus();

	$("#searchBtn").click(function(){
		searchUsers();
	});

	$("body").keypress(function(arg){
		if(arg.keyCode === 13)
		{
			searchUsers();
		}
	})
});