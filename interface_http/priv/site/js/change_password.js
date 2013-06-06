$(document).ready(function() {
	$("#old_pwd").focus();
	
	$("#changePwdForm").validate({rules:{
		new_pwd: {
			minlength: 6
		},
		new_pwd2: {
			equalTo: "#new_pwd",
			minlength: 6
		}
	}});
});
