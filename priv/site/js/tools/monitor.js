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
        $("#autoRefreshIntervalTextbox").change(auto_refresh_change);

	window.ajaxContainer = {}; // store ajax request
	window.autoRefreshIntervalId = 0;

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
    window.clearInterval(window.autoRefreshIntervalId);
	
    var checked = document.getElementById("autoRefreshCheckbox").checked;
	if(checked)
	{
		window.autoRefreshIntervalId = window.setInterval(function() { refreshImages(false) }, parseInt($("#autoRefreshIntervalTextbox").val())*1000);
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
	var $memory_container = $("#memoryContainer");
	var $cpuLoad_container = $("#cpuLoadContainer");
	var $cpuUtil_container = $("#cpuUtilContainer");

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

		$memory_container.text("");
		var memory_spinner = new Spinner(opts).spin($memory_container[0]);
		$cpuLoad_container.text("");
		var cpuLoad_spinner = new Spinner(opts).spin($cpuLoad_container[0]);
		$cpuUtil_container.text("");
		var cpuUtil_spinner = new Spinner(opts).spin($cpuUtil_container[0]);
	}

	if(window.ajaxContainer["memory"])
	{
		window.ajaxContainer["memory"].abort();
	}
	window.ajaxContainer["memory"] = $.ajax({
		url: "/monitor/rrd/graph/memory",
		type: "POST",
		data: { fromDateTime: fromDateTime, toDateTime: toDateTime },
		success: function(data, textStatus, jqXHR)
		{
			$memory_container.text("");
			if(data.success)
			{
				$memory_container.html("<img src=\"" + data.data.url + "\" />");
				$("#lastUpdated").text("Last updated: " + $.utils.getTimeStr());
			}
			else
			{
				$memory_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$memory_container.text("");
				$memory_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			if(showSpiner)
			{
				memory_spinner.stop();
			}	
		}
	});


	if(window.ajaxContainer["cpuLoad"])
	{
		window.ajaxContainer["cpuLoad"].abort();
	}
	window.ajaxContainer["cpuLoad"] = $.ajax({
		url: "/monitor/rrd/graph/cpu_load",
		type: "POST",
		data: { fromDateTime: fromDateTime, toDateTime: toDateTime },
		success: function(data, textStatus, jqXHR)
		{
			$cpuLoad_container.text("");
			if(data.success)
			{
				$cpuLoad_container.html("<img src=\"" + data.data.url + "\" />");
				$("#lastUpdated").text("Last updated: " + $.utils.getTimeStr());
			}
			else
			{
				$cpuLoad_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$cpuLoad_container.text("");
				$cpuLoad_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			if(showSpiner)
			{
				cpuLoad_spinner.stop();
			}
		}
	});


	if(window.ajaxContainer["cpuUtil"])
	{
		window.ajaxContainer["cpuUtil"].abort();
	}
	window.ajaxContainer["cpuUtil"] = $.ajax({
		url: "/monitor/rrd/graph/cpu_util",
		type: "POST",
		data: { fromDateTime: fromDateTime, toDateTime: toDateTime },
		success: function(data, textStatus, jqXHR)
		{
			$cpuUtil_container.text("");
			if(data.success)
			{
				$cpuUtil_container.html("<img src=\"" + data.data.url + "\" />");
				$("#lastUpdated").text("Last updated: " + $.utils.getTimeStr());
			}
			else
			{
				$cpuUtil_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$cpuUtil_container.text("");
				$cpuUtil_container.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			if(showSpiner)
			{
				cpuUtil_spinner.stop();
			}
		}
	});

	return false;
}

