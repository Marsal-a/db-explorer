$(document).keyup(function(event) {
    if ($("#password_pg").is(":focus") && (event.key == "Enter")) {
        $("#submit_pg_login").click();
    }
});

$(document).keyup(function(event) {
    if ($("#modal_pw").is(":focus") && (event.key == "Enter")) {
        $("#modal_submit_login").click();
    }
});
