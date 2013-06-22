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

		switch1Btn_click: function(event)
		{
			var deviceId = $(event.target).parent().parent().attr("id");
			var status = 1;
			if($(event.target).text() == "on")
			{
				status = 0;
			}

			this.update_switch1_status(deviceId, status);
		},

		bindEvents: function()
		{
			var me = this;

			$(".switch1Btn").unbind().click(function(event){me.switch1Btn_click(event)});
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


		update_switch1_status: function(deviceId, status) 
		{
			var msg = {
				cmd: "update_switch_status",
				sid: $.cookie(this.sessionCookieId),
				data: {"device_id": deviceId, "switch_id": "switch1", "status": status}
			};
		    this.socket_send_msg(msg);
		},

		update_switch1_status_success: function(data) 
		{
		},

		update_switch1_status_error: function(data) 
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
			var me = this;

			for(var i=0; i< data.length; i++)
			{
				this.display_device(data[i]);
			}

			this.bindEvents();
		},

		list_online_devices_error: function(data) 
		{
			window.alert(data);
		},


		device_status_changed_success: function(data) 
		{
			this.display_device(data);
			this.bindEvents();
		},


		display_device: function(data)
		{			
			if(data.values.online)
			{
				if($("#"+data.device_id).size() === 0)
				{
					// create the element
					$("#" + data.type + "Template").tmpl(data).appendTo($("#devicesContainer"));
				}
				else
				{
					// update the element
					$device = $("#"+data.device_id);
					$device.find(".deviceName").text(data.name);
					for(var i = 0 i < data.values.length; i++)
					{
						$device.find(data.values[i].key).text(data.values[i].value);
					}
				}
			}
			else
			{
				// delete the element
				$("#"+data.device_id).remove();
			}
		},

		//============ web socket ======================================================================
		socket_init: function()
		{
			if (window.WebSocket) this.WebSocket = window.WebSocket;
		    if (!this.WebSocket && window.MozWebSocket) this.WebSocket = window.MozWebSocket;
		    if (!this.WebSocket)
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
				window.setTimeout(function(){me.socket_connect();}, 6000);
			};

			var url = window.location.host + "/device/socket";
			if(window.location.protocol == "http:")
			{
				url = "ws://" + url;
			}
			else
			{
				url = "wss://" + url;
			}
			
		    this.webSocket = new this.WebSocket(url);
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
