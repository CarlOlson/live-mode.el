function getFontSize(node) {
  var size = window
    .getComputedStyle(node, null)
    .getPropertyValue('font-size');
  return parseInt(size);
}

function changeFontSizeBy(diff) {
  diff = parseInt(diff) || 0;
  return function() {
    var buffer    = document.getElementById('buffer');
    var font_size = document.getElementById('font-size');
    if (buffer != null) {
      var new_size = getFontSize(buffer) + diff;
      buffer.style.fontSize = new_size + "px";
      font_size.value = new_size;
    }
  }
}

(function () {
  var increment = document.getElementById('font-inc');
  var decrement = document.getElementById('font-dec');

  increment.addEventListener('click', changeFontSizeBy( 1));
  decrement.addEventListener('click', changeFontSizeBy(-1));

  changeFontSizeBy(0)();
})();
