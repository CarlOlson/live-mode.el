function getFontSize() {
  var cookies = document.cookie.split(';');
  for (var i = 0; i < cookies.length; i++)
    if (cookies[i].match(/^ ?font-size=/))
      return parseInt(cookies[i].split('=')[1]);
}

function changeFontSizeBy(diff) {
  diff = parseInt(diff) || 0;

  return function() {
    var size = (getFontSize() || 18) + diff;
    if (size < 1) return;

    document.cookie = 'font-size=' + size;

    var buffer = document.getElementById('buffer');
    if (buffer != null)
      buffer.style.fontSize = size + "px";

    var font_size = document.getElementById('font-size');
    if (font_size != null)
      font_size.value = size;
  }
}

(function () {
  var increment = document.getElementById('font-inc');
  var decrement = document.getElementById('font-dec');

  increment.addEventListener('click', changeFontSizeBy( 1));
  decrement.addEventListener('click', changeFontSizeBy(-1));

  changeFontSizeBy(0)();
})();
