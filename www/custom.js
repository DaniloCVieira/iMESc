Shiny.addCustomMessageHandler('triggerSnapshot', function(message) {
  var plot = document.getElementById('plot');
  if (plot) {
    Plotly.downloadImage(plot, {format: 'png', filename: 'plot_snapshot'});
  }
});
