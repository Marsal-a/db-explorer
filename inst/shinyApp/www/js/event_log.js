$(document).on('shiny:inputchanged', function(event) {
    if (
      event.name != 'changed_value' & 
      event.name!='changed' & 
      !event.name.includes('_state') & 
      !event.name.includes('navig_dataviewer_') &
      !event.name.includes('_hidden') 
    ) {
      Shiny.setInputValue('changed', event.name);
      var inputValue = event.value;
      var combinedValue = event.name + ':' + inputValue;
      if (event.name.includes('modal_pw')) {
        Shiny.setInputValue('changed_value', event.name + ':');
      }else{
        Shiny.setInputValue('changed_value', combinedValue);
      }
  }
});