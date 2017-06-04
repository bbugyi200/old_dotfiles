function getPageZoom()
{
  //RETURN: 1.0 for 100%, and so on
  var zoom = 1;

  try
  {
    var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
    svg.setAttribute('version', '1.1');
    var body = $("#body")[0];
    console.log(body);
    body.appendChild(svg);
    zoom = svg.currentScale;
    body.removeChild(svg);
    console.log("Zoom: " + zoom);
  }
  catch(e)
  {
    console.error("Zoom method failed: " + e.message);
  }

  return zoom;
}

$(document).ready(function() {
  var zoom = window.devicePixelRatio;
  console.log(zoom);
  var height = 544;
  var width = 619;
  var scaled_height = height * zoom;
  scaled_height = ~~scaled_height;
  var scaled_width = width * zoom;
  scaled_width = ~~scaled_width;
  var body = $("#body");
  body.css("width", scaled_width + "px");
  body.css("height", scaled_height + "px");
});