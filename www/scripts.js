Shiny.addCustomMessageHandler("mapReset", function(p) {
  HTMLWidgets.getInstance(map)
    .getMap()
    .invalidateSize();
});

Shiny.addCustomMessageHandler("dismiss", function(p) {
  location.href = "http://www.google.com";
});

Shiny.addCustomMessageHandler("setCss", function(p) {
  $("#" + p[0]).css(p[1], p[2]);
});

Shiny.addCustomMessageHandler("fullscreen", function(p) {
  var el = document.documentElement;
  if (p.mode === "enter") {
    if (el.requestFullscreen) el.requestFullscreen();
    else if (el.mozRequestFullScreen) el.mozRequestFullScreen();
    else if (el.msRequestFullscreen) el.msRequestFullscreen();
    else if (el.webkitRequestFullscreen) el.webkitRequestFullscreen();
  } else {
    if (document.exitFullscreen) {
      document.exitFullscreen();
    } else if (document.mozCancelFullScreen) {
      document.mozCancelFullScreen();
    } else if (document.webkitExitFullscreen) {
      document.webkitExitFullscreen();
    }
  }
});

function initLightbox() {
  $('#imageWrapper div').click(function(e) {
    $('#lightbox').removeClass('hidden');
    Shiny.setInputValue('currLight', parseInt(e.target.id.substring(3, 4)));
  });
  $('body').keydown(function(e) {
    var targ = 'Page';
    if (!$('#lightbox').hasClass('hidden')) targ = 'Light';
    if (e.which === 37) $('#prev' + targ).click();
    else if (e.which === 39) $('#next' + targ).click();
  });
}
