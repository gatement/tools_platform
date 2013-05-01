$(document).ready(function()
{
	$("#fromDateTimeField").val($.utils.getTimeStr($.utils.getTimeByHour(-1))); // 1 hour ago
	$("#toDateTimeField").val($.utils.getTimeStr()); // now
	$("#fromDateTimeField").datetimepicker({
		dateFormat: "yymmdd",
		timeFormat: "hh:mm",
		changeMonth: true
	});
	$("#toDateTimeField").datetimepicker({
		dateFormat: "yymmdd",
		timeFormat: "hh:mm",
		changeMonth: true
	});

	$("#showBtn").click(show_button_click);
	$("#refreshBtn").click(refresh_button_click);
	$("#autoRefreshCheckbox").change(auto_refresh_change);

	window.ajaxContainer = {}; // store ajax request
	window.autoRefreshIntervalId = 0;
	window.autoRefreshInterval = 90000;

	show_button_click();
});

function show_button_click()
{
	var fromDateTime = $("#fromDateTimeField").val();
	var toDateTime = $("#toDateTimeField").val();

	updateRRDToolImage(fromDateTime, toDateTime, true);
}

function refresh_button_click()
{
	refreshImages(true);
}

function auto_refresh_change()
{
	var checked = $("#autoRefreshCheckbox").attr("checked");
	if(checked)
	{
		window.autoRefreshIntervalId = window.setInterval(function() { refreshImages(false) }, window.autoRefreshInterval);
	}
	else
	{
		window.clearInterval(window.autoRefreshIntervalId)
	}
}

function refreshImages(showSpiner)
{
	var timeWindow = $("#timeWindowSelect").val();
	var minutes = parseInt(timeWindow);

	var fromDateTime = $.utils.getTimeStr($.utils.getTimeByMinute(-minutes));
	var toDateTime = $.utils.getTimeStr(); // now

	updateRRDToolImage(fromDateTime, toDateTime, showSpiner);
}

function updateRRDToolImage(fromDateTime, toDateTime, showSpiner)
{
	var agentId = $("#agentIdField").val();

	var $time_container = $("#timeContainer");
	var $loss_container = $("#lossContainer");

	$time_container.text("");
	$loss_container.text("");

	if(showSpiner)
	{
		var opts = {
			lines: 10, // The number of lines to draw
			length: 3, // The length of each line
			width: 2, // The line thickness
			radius: 4, // The radius of the inner circle
			corners: 1, // Corner roundness (0..1)
			rotate: 0, // The rotation offset
			color: '#000', // #rgb or #rrggbb
			speed: 1, // Rounds per second
			trail: 60, // Afterglow percentage
			shadow: false, // Whether to render a shadow
			hwaccel: false, // Whether to use hardware acceleration
			className: 'spinner', // The CSS class to assign to the spinner
			zIndex: 2e9, // The z-index (defaults to 2000000000)
			top: 30, // Top position relative to parent in px
			left: -9 // Left position relative to parent in px
		};
		var time_spinner = new Spinner(opts).spin($time_container[0]);
		var loss_spinner = new Spinner(opts).spin($loss_container[0]);
	}

	if(window.ajaxContainer["time"])
	{
		window.ajaxContainer["time"].abort();
	}
	window.ajaxContainer["time"] = $.ajax({
		url: "/statistic/ping/time",
		type: "POST",
		data: { agentId: agentId, fromDateTime: fromDateTime, toDateTime: toDateTime },
		success: function(data, textStatus, jqXHR)
		{
			if(data.success)
			{
				$time_container.html("<img src=\"" + data.data.url + "\" />");
				$("#lastUpdated").text("Last updated: " + $.utils.getTimeStr());
			}
			else
			{
				$time_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$time_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			if(showSpiner)
			{
				time_spinner.stop();
			}	
		}
	});

	if(window.ajaxContainer["loss"])
	{
		window.ajaxContainer["loss"].abort();
	}
	window.ajaxContainer["loss"] = $.ajax({
		url: "/statistic/ping/loss",
		type: "POST",
		data: { agentId: agentId, fromDateTime: fromDateTime, toDateTime: toDateTime },
		success: function(data, textStatus, jqXHR)
		{
			if(data.success)
			{
				$loss_container.html("<img src=\"" + data.data.url + "\" />");
				$("#lastUpdated").text("Last updated: " + $.utils.getTimeStr());
			}
			else
			{
				$loss_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$loss_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			if(showSpiner)
			{
				loss_spinner.stop();
			}	
		}
	});

	return false;
}

