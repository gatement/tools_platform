$(document).ready(function() {
	$("#id").focus();
	
	$("#registerForm").validate({rules:{
		pwd: {
			minlength: 6
		},
		pwd2: {
			equalTo: "#pwd",
			minlength: 6
		}
	}});
});
