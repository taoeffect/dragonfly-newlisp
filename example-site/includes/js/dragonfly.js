$(document).ready(function () {
	window.scrollTo(0, 1); // scrollTo function for iPhone support
	$("a.menu_toggle").click(function () {
		$(this).parent().siblings().children("div.menu:visible").hide(); // hide the other visible menus
		$(this).next().toggle(); // toggle our menu
		return false;
	});
});
