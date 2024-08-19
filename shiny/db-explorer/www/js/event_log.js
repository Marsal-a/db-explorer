$(document).on('shiny:inputchanged', function(event) {
    if (event.name != 'changed') {
    var inputValue = event.value;
    
    var combinedValue = event.name + ':' + inputValue;
    
    
    Shiny.setInputValue('changed', event.name);
  }
});
$(document).on('shiny:inputchanged', function(event) {
    if (event.name != 'changed_value') {
      var inputValue = event.value;
      var combinedValue = event.name + ':' + inputValue;
      if (event.name.includes('modal_pw')) {
        Shiny.setInputValue('changed_value', event.name + ':');
      }else{
        Shiny.setInputValue('changed_value', combinedValue);
      }
  }
});