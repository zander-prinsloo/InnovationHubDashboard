//$( document ).ready(function() {
//
//});
// www/fade.js
$(function(){
  const $plot = $('#dd_col-deepdive_plot');
  window.addEventListener('scrollytell:step', e => {
     $plot.css({opacity:0}).animate({opacity:1}, 400);
  });
});
