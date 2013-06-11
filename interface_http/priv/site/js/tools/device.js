if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Device()
	{
		var me = this;

		this.sessionCookieId = "usr_sid",
		this.firstLoading = true;
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
			for(var i=0; i< data.length; i++)
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

					// update the voltage display
					$device = $("#"+data.sn);
					this.display_device_voltage($device, data.voltage);
				}
				else
				{
					// update the element
					$device = $("#"+data.sn);
					$device.find(".deviceName").text(data.name);
					$device.find(".deviceSn").text(data.sn);
					this.display_device_voltage($device, data.voltage);
				}
			}
			else
			{
				// delete the element
				$("#"+data.sn).remove();
			}
		},

		display_device_voltage: function($device, voltage)
		{
			var width = window.parseInt(voltage * 100 / 1023);
			var bgColor = this.get_device_voltage_bg_color(width);
			var widthStr = $.utils.formatStr("{0}%", width);
			var voltageText = $.utils.formatStr("{0}V / {1}", voltage, widthStr);				
			$device.find(".voltageText").text(voltageText);

			//$device.find(".voltageProgress").animate({"width": widthStr, "background-color": bgColor});
			$device.find(".voltageProgress").css("width", widthStr);
			$device.find(".voltageProgress").css("background-color", bgColor);
		},

		get_device_voltage_bg_color: function(voltagePercentage)
		{
			var bgColor = "rgb(0,210,0)"; //#0099FF

			if(voltagePercentage < 30){
				var b = 256 - window.parseInt(voltagePercentage*256/100);
				var g = window.parseInt(b * 0.7);
				bgColor = $.utils.formatStr("rgb(0, {0}, {1})", g, b);
			}
			else if(voltagePercentage > 90){
				var r = window.parseInt(voltagePercentage*256/100);
				bgColor = $.utils.formatStr("rgb({0}, 100, 100)", r);
			}

			return bgColor;
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
					me.list_online_devices();
				}

				// upate session to the new socket PID whenever connect(re-connect)
				me.update_socket();
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

				// log
				if(console && console.log)
				{
					console.log("received: %o", msg);
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
