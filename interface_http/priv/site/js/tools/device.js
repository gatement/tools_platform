if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Device()
	{
		var me = this;

		this.sessionCookieId = "usr_sid",
		this.firstLoading = false;
	};

	$.extend(Device.prototype, {
		init: function()
		{
			this.socket_init();
			this.socket_connect();
		},


		//============ web socket operations ===========================================================
		update_socket: function() 
		{
			var msg = {
				cmd: "update_socket",
				sid: $.cookie(this.sessionCookieId),
				data: ""
			};
		    this.socket_send_msg(msg);
		},

		update_socket_success: function(data) 
		{
		},

		update_socket_error: function(data) 
		{			
			window.alert(data);
		},

		list_online_devices: function()
		{
			var msg = {
				cmd: "list_online_devices",
				sid: $.cookie(this.sessionCookieId),
				data: ""
			};
		    this.socket_send_msg(msg);
		},

		list_online_devices_success: function(data) 
		{
			for(int i=0; i< data.length; i++)
			{
				this.update_device_display(data[i]);
			}
		},

		list_online_devices_error: function(data) 
		{
			window.alert(data);
		},

		device_status_changed_success: function(data) 
		{
			this.update_device_display(data);
		},

		update_device_display: function(data)
		{			
			if(data.is_online)
			{
				if($("#"+data.sn).size() === 0)
				{
					// create the element
					$("#deviceTemplate").tmpl(data).appendTo($("#devicesContainer"));
				}
				else
				{
					// update the element
					$device = $("#"+data.sn);
					$device.find(".deviceName").text(data.name);
					$device.find(".deviceSn").text(data.sn);

					var width = data.voltage * 100 / 1023;	
					$device.find(".voltageProgress").animate({width, width + "%"});
				}
			}
			else
			{
				// delete the element
				$("#"+data.sn).remove();
			}
		},

		//============ web socket ======================================================================
		socket_init: function()
		{
			var isSafari = navigator.userAgent.indexOf("Safari") != -1 && navigator.userAgent.indexOf("Chrome") === -1;

			if (window.WebSocket) this.WebSocket = window.WebSocket;
		    if (!this.WebSocket && window.MozWebSocket) this.WebSocket = window.MozWebSocket;
		    if (!this.WebSocket || isSafari)
		    {
		    	alert("Your browser is not supported. Please use Firefox or Google Chrome.");
		    }
		},

		socket_connect: function()
		{
			var me = this;

			var socket_on_open = function()
			{
				if(me.firstLoading)
				{
					me.firstLoading = false;

					// do something on first loading

				}
				else // reconnect
				{
					me.update_socket(); // upate session to the new socket PID
				}
			},
			socket_on_message = function(msg)
			{
				if(msg.data)
				{
					var response = window.JSON.parse(msg.data);
					if(response.success)
					{
						me[response.cmd + "_success"].apply(me, [response.data]);
					}
					else
					{
						me[response.cmd + "_error"].apply(me, [response.data]);
					}
				}
			},
			socket_on_close = function()
			{
				if(console && console.log)
				{
					console.log("Socket is closed, is reconnecting...");
				}
				window.setTimeout(function(){me.socket_connect();}, 3000);
			};


		    this.webSocket = new this.WebSocket("ws://" + window.location.host + "/device/socket");
		    this.webSocket.onopen = socket_on_open;
		    this.webSocket.onmessage = socket_on_message;
		    this.webSocket.onclose = socket_on_close;
		},

		socket_send_msg: function(msgObj)
		{
			var me = this;

			if(this.webSocket && this.webSocket.readyState === this.webSocket.OPEN && msgObj)
			{
				var msg = window.JSON.stringify(msgObj);
				var sendResult = this.webSocket.send(msg);
				
				if(sendResult === false)
				{
					if(console && console.log)
					{
						console.log("Communication error, is retrying...");
					}
					window.setTimeout(function(){me.socket_send_msg(msgObj);}, 5000);
				}
			}
			else
			{
				if(console && console.log)
				{
					console.log("Communication error, is retrying...");
				}
				window.setTimeout(function(){me.socket_send_msg(msgObj);}, 5000);
			}

			// log
			if(console && console.log)
			{
				console.log("sending: %o", msgObj);
			}
		}
	});

	tp.Device = Device;
	
})(jQuery, tp);


$(document).ready(function() 
{
	tp.device = new tp.Device();
	tp.device.init();
});
