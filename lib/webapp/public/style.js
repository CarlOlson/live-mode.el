
function getHighlightStyleSheets() {
    var nodes = document.getElementsByTagName('link');
    var rtn = []
    for (var i = 0; i < nodes.length; i++)
	if (nodes[i].rel.match('stylesheet') &&
	    nodes[i].href.match('highlight'))
	    rtn[rtn.length] = nodes[i];
    return rtn;
}

function disableStyleSheets(sheets) {
    for (var i = 0; i < sheets.length; i++)
	sheets[i].disabled = true;
}

function loadHighlightStyle(name){
    name = name.toLowerCase().replace(/(\n|\t| )/g, '');
    document.cookie = 'style=' + name;
    var old_styles = getHighlightStyleSheets();

    // disable old styles
    disableStyleSheets(old_styles);

    // check if already loaded
    for (var i = 0; i < old_styles.length; i++)
	if (old_styles[i].href.match(name)) {
	    old_styles[i].disabled = false;
	    return;
	}

    // load new styles
    var style = document.createElement("link");
    style.rel = 'stylesheet';
    style.href =
	'//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.8.0/styles/' +
	name +
	'.min.css';
    document
	.getElementsByTagName("body")[0]
	.appendChild(style);
}

function styleButtonHandler (event) {
    // TODO: this should be safe, right?
    loadHighlightStyle(event.target.innerHTML);
}

(function () {
    var buttons = document.getElementsByClassName('style-button');

    for (var i = 0; i < buttons.length; i++)
	buttons[i].addEventListener('click', styleButtonHandler);
})();
