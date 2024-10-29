$(document).on('shiny:connected', function() {
  Shiny.setInputValue('windowHeight', window.innerHeight);
  Shiny.setInputValue('windowWidth', window.innerWidth);
});
$(window).resize(function() {
  Shiny.setInputValue('windowHeight', window.innerHeight);
  Shiny.setInputValue('windowWidth', window.innerWidth);
});