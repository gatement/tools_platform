if(!tp)
{
	var tp = {};
}

(function($, tp){

	function Device()
	{
		var me = this;

		this.sessionCookieId = "usr_sid";
	};

	$.extend(Device.prototype, {
		init: function()
		{
			this.socket_init();
			this.socket_connect();

			$(".permissionListItemDeleteButton").click(function(event){me.permissionListItemDeleteButton_click(event)});
			$("#permissionAddButton").click(function(event){me.permissionAddButton_click(event)});
		},

		permissionListItemDeleteButton_click: function(event)
		{

		},

		permissionAddButton_click: function(event)
		{
			
		},

		load_permissions: function()
		{

		},

		switch1_click: function(event)
		{
			var deviceId = $(event.target).parent().parent().attr("id");
			var status = 1;
			if($(event.target).text() == "on")
			{
				status = 0;
			}

			this.update_switch_status(deviceId, "switch1", status);
		},

		poweroff_click: function(event)
		{
			var deviceId = $(event.target).parent().parent().attr("id");
			var cmd = "poweroff";

			this.send_command(deviceId, cmd);
		},

		restart_click: function(event)
		{
			var deviceId = $(event.target).parent().parent().attr("id");
			var cmd = "restart";

			this.send_command(deviceId, cmd);
		},

		permission_click: function(event)
		{
			var me = this;

			var deviceId = $(event.target).parent().parent().attr("id");			

			this.load_permissions();
			$("#permissionMgmtDialog").dialog({modal: true, zIndex: 200000, width: 620, minWidth: 620});
			$("#permissionAddTextbox").focus();
		},

		bindEvents: function()
		{
			var me = this;

			$(".switch1").unbind().click(function(event){me.switch1_click(event)});
			$(".poweroff").unbind().click(function(event){me.poweroff_click(event)});
			$(".restart").unbind().click(function(event){me.restart_click(event)});
			$(".permission").unbind().click(function(event){me.permission_click(event)});
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


		update_switch_status: function(deviceId, switchName, status) 
		{
			var msg = {
				cmd: "update_switch_status",
				sid: $.cookie(this.sessionCookieId),
				data: {"device_id": deviceId, "switch": switchName, "status": status}
			};
		    this.socket_send_msg(msg);
		},

		update_switch_status_success: function(data) 
		{
		},

		update_switch_status_error: function(data) 
		{			
			window.alert(data);
		},


		send_command: function(deviceId, cmd) 
		{
			var msg = {
				cmd: "send_command",
				sid: $.cookie(this.sessionCookieId),
				data: {"device_id": deviceId, "cmd": cmd}
			};
		    this.socket_send_msg(msg);
		},

		send_command_success: function(data) 
		{
		},

		send_command_error: function(data) 
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
			
			$("#devicesContainer").empty();

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
					for(var key in data.values)
					{
						data[key] = data.values[key];
					}

					$("#" + data.type + "Template").tmpl(data).appendTo($("#devicesContainer"));
				}
				else
				{
					// update the element
					$device = $("#"+data.device_id);
					$device.find(".deviceName").text(data.name);
					for(var key in data.values)
					{
						$device.find("."+key).text(data.values[key]);
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
				// upate session to the new socket PID whenever connect(re-connect)
				me.update_socket();

				me.list_online_devices();
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
