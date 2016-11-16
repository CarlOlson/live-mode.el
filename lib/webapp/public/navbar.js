function getFontSize() {
  var cookies = document.cookie.split(';');
  for (var i = 0; i < cookies.length; i++)
    if (cookies[i].startsWith('font-size'))
      return parseInt(cookies[i].split('=')[1]);
}

function changeFontSizeBy(diff) {
  diff = parseInt(diff) || 0;
  var size = getFontSize() || 18;

  return function() {
    size = size + diff;
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
