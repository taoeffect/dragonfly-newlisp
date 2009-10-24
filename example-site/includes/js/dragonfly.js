/* scrollTo function for iPhone support */
window.onload = function () {
		this.scrollTo(0, 1);
};
	
/* toggleMenu function for Dragonfly User Guide Menu */
function toggleMenu() {

	/* very basic without animation */
	if ($('#menu').is(':visible')) {
	    $('#menu').hide();
	} else {
	    $('#menu').show();
	}

}


/* simple AJAX Request */

function AjaxRequest(url, completeFunction) {
	var request = this;
	var request_url = url;
	
	this.callback = completeFunction || function () { };
	this.post = function(params) {
		request.http = null;
		if (window.XMLHttpRequest) {
			request.http = new XMLHttpRequest();
		} else {
			request.http = new ActiveXObject("Microsoft.XMLHTTP");
		}
	
	if (request.http == null) {
		return false;
	} else {
		request.http.onreadystatechange = function() {
			if (request.http.readyState == 4 && request.http.status == 200) {
				request.callback(request.http.responseText, request.http.status, request.http.responseXML);
				request.http = null;
			}
		}

		request.http.open("POST", request_url, true);
		request.http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
		request.http.setRequestHeader("Content-Length", params.length);
		request.http.send(params);
		return true;
	}	
	
	}
}




