function changeActiveTabTitle(new_title) {
  //var tabsetId = $('.nav-tabs').attr('id') ;
  //$('#' + tabsetId + ' li.active a').text(new_title);
  //var tabsetId = $('.nav-pills').attr('id') ;
  //$('#' + tabsetId + ' li.active a').text(new_title);
  
}

function addCloseButtonToActiveTab(new_title, removeInput) {
  
  var tabsetId = $('.nav-pills').attr('id');
  var activeTab = $('#' + tabsetId + ' li.active a');
  var dataValue = activeTab.data('value');

  $('#' + tabsetId + ' li.active a').text(new_title);
  
  var closeButton = $('<span>').addClass('close-button').html('<span class="fa fa-remove" style="margin-left: 5px;"></span>');
  closeButton.on('click', function() {
    Shiny.setInputValue(removeInput, dataValue, {priority: 'event'});
  });
  
  activeTab.append(closeButton);
}