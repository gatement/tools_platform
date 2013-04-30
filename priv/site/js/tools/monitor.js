if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Monitor()
	{
		var me = this;
	};

	$.extend(Monitor.prototype, {
		init: function()
		{
			
		}

		//============ Helpers ======================================================================

	});

	tp.Monitor = Monitor;
	
})(jQuery, tp);


$(document).ready(function() 
{
	tp.monitor = new tp.Monitor();
	tp.monitor.init();
});
