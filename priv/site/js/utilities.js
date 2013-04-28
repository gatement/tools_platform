if(!jt) 
{
	var jt = {};
}
jt.utilities = {};

jt.utilities.formatStr = function(str)
{
	var args = arguments;

	return str.replace(/{(\d+)}/g, function(match, number)
	{
		var num = parseInt(number);
		return typeof args[num + 1] != 'undefined' ? args[num + 1] : '{' + number + '}';
	});
}