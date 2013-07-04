if(!$) 
{
	var $ = {};
}
$.utils = {};

// {0}, {1} are the place holder
$.utils.formatStr = function(str)
{
	var args = arguments;

	return str.replace(/{(\d+)}/g, function(match, number)
	{
		var num = parseInt(number);
		return typeof args[num + 1] != 'undefined' ? args[num + 1] : '{' + number + '}';
	});
}

$.utils.getTimeStr = function(time)
{
	if(!time)
	{
		var time = new Date();
	}

	var monthStr = (time.getMonth() + 1).toString();
	if(monthStr.length === 1)
	{
		monthStr = "0" + monthStr;
	}

	var dateStr = time.getDate().toString();
	if(dateStr.length === 1)
	{
		dateStr = "0" + dateStr;
	}

	var hourStr = time.getHours().toString();
	if(hourStr.length === 1)
	{
		hourStr = "0" + hourStr;
	}

	var minStr = time.getMinutes().toString();
	if(minStr.length === 1)
	{
		minStr = "0" + minStr;
	}

	return time.getFullYear().toString() + monthStr + dateStr + " " + hourStr + ":" + minStr;
}

$.utils.getTimeByMinute = function(minute)
{
	var time = new Date();
	time.setMinutes(time.getMinutes() + minute);
	return time;
}

$.utils.getTimeByHour = function(hour)
{
	var time = new Date();
	time.setHours(time.getHours() + hour);
	return time;
}

$.utils.getTimeByDay = function(day)
{
	var time = new Date();
	time.setDate(time.getDate() + day);
	return time;
}