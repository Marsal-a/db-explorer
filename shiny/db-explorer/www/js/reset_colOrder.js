Shiny.addCustomMessageHandler("reset_colorder", function(string_order) {
  Shiny.setInputValue("columnClicked", "init_reserved_string");
});

Shiny.addCustomMessageHandler("reset_colorder_navig", function(input_name) {
  console.log("reset_colorder_navig triggered");
  Shiny.setInputValue(input_name, "init_reserved_string");
});

Shiny.addCustomMessageHandler("resetShinyInput", function(message) {
  Shiny.setInputValue(message.input_name, message.input_value);
});
