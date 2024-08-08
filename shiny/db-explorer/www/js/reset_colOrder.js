Shiny.addCustomMessageHandler("reset_colorder", function(string_order) {
  Shiny.setInputValue("columnClicked", "init_reserved_string");
});