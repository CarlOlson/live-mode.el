var UPDATE_INTERVAL = 125;

var socket;
var host = 'ws:' +
	   window.location.hostname +
	   ':3001' +
	   window.location.pathname;
var bufferText = '';
var bufferMode = '';
var modified = false;
var hljs;

function connect() {
  try {
    socket = new WebSocket(host);

    socket.onmessage = function(event) {
      processEvent(event);
    }
  } catch(exception) {
    console.log(exception);
  }
}

function processEvent(event) {
  var cmd = JSON.parse(event.data);
  switch(cmd.event) {
    case 'set':
      setBuffer(cmd.text);
      setMode(cmd.mode);
      break;
    case 'update':
      updateBuffer(cmd.start,
		   cmd.length,
		   cmd.text);
      break;
  }
  modified = true;
}

function setBuffer(text) {
  bufferText = text || '';
}

function setMode(mode) {
  bufferMode = mode || '';
}

function updateBuffer(start, length, insertText) {
  // buffer index starts at 1
  start -= 1;

  bufferText =
    bufferText.substring(0, start) +
    insertText +
    bufferText.substring(start + length);
}

function refresh() {
  if (!modified)
    return;

  modified = false;

  var buffer = document.getElementById("buffer");

  if (bufferText.length == 0) {
    buffer.visible = false;
  } else {
    buffer.innerHTML = escapeHTML(bufferText);
    buffer.setAttribute('class', bufferMode);
    if (hljs != null)
      hljs.highlightBlock(buffer);
  }
}

var entityMap = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;',
  '"': '&quot;',
  "'": '&#39;',
  '`': '&#x60;',
  '=': '&#x3D;',
  '/': '&#x2F;'
};

// stolen from mustache.js
function escapeHTML(string) {
  return String(string).replace(/[&<>"'`=\/]/g,
				function (s) {
				  return entityMap[s];
				});
}

connect();
setInterval(refresh, UPDATE_INTERVAL);
