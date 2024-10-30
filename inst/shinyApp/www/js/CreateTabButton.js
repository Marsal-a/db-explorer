$(document).ready(function() {
    var navig_add_button = "<li class='list_button'>" +
                 "<button type='button' class='btn btn-success' " +
                 "onclick='Shiny.onInputChange(\"navig_create_tab\", Math.random())'>" +
                 "<i class='fa fa-plus'></i>" +
                 "</button>" +
                 "</li>";
    $("#Navig_tabset_panel").first().append(navig_add_button);
    
    var sql_add_button = "<li class='list_button'>" +
                 "<button type='button' class='btn btn-success' " +
                 "onclick='Shiny.onInputChange(\"sql_create_tab\", Math.random())'>" +
                 "<i class='fa fa-plus'></i>" +
                 "</button>" +
                 "</li>";
    $("#ConsoleSQL_tabset_panel").first().append(sql_add_button);
    
});
