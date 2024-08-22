$(document).ready(function() {
      var button = "<li class='list_button'><button type='button' class='btn btn-success' onclick='Shiny.onInputChange(\"create_tab\", Math.random())'><i class='fa fa-plus'></i></button></li>";
                $("#TABSETPANEL").first().append(button);
  });
  
