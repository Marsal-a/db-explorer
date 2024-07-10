$(document).keyup(function(event) {
    if ($("#password_pg").is(":focus") && (event.key == "Enter")) {
        $("#submit_pg_login").click();
    }
});