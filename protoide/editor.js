/**
 * Very basic editor
 *
 * Invariants: 
 * - every line ends on a linebreak
 * - There is always at least one line
 */

var cursorHighlighted = 1;
var editor = { 
    cursorPos: {l:1, c:1}
  , source:    ["\n"]
  }

String.prototype.splice = function( idx, rem, s ) {
    return (this.slice(0,idx) + s + this.slice(idx + Math.abs(rem)));
};

window.onload = function() {
  // Start cursor blinking 
  window.setInterval(blinkCursor, 500);

  // Start editor
  var body = document.getElementsByTagName("body")[0];
  body.onkeyup    = editorOnKeyUp;

  // Make sure backspace doesn't load the previous page
  body.onkeydown  = trapKeys; 
  body.onkeypress = trapKeys;
}

function trapKeys(e) {
  if(e.keyCode == 8) {
    return false;
  }

  return true;
}

function lineLength(l) {
  return editor.source[l - 1].length;
}

function numberOfLines() {
  return editor.source.length;
}

function setCursorPos(l, c) {
  // Make sure l and c are within range
  if(l < 1)               l = 1;
  if(l > numberOfLines()) l = numberOfLines();

  if(c < 1)             c = 1;
  if(c > lineLength(l)) c = lineLength(l);

  editor.cursorPos = {l:l, c:c};
}

function spliceAtCursor(rem, ins) {
  var l = editor.cursorPos.l;
  var c = editor.cursorPos.c;

  editor.source[l - 1] = editor.source[l - 1].splice(c - 1, rem, ins);
}

function editorOnKeyUp(e) {
  var chr;

  if(e.keyCode == 13) { // enter
    editor.source.splice(editor.cursorPos.l, 0, "\n"); // splice on arrays modifies the array
    setCursorPos(editor.cursorPos.l + 1, 1);
  } else if (e.keyCode == 8) { // backspace {
    setCursorPos(editor.cursorPos.l, editor.cursorPos.c - 1);
    spliceAtCursor(1, "");
  } else if (e.keyCode == 37) { // left arrow 
    setCursorPos(editor.cursorPos.l, editor.cursorPos.c - 1);
  } else if (e.keyCode == 38) { // up arrow 
    setCursorPos(editor.cursorPos.l - 1, editor.cursorPos.c);
  } else if (e.keyCode == 39) { // right arrow 
    setCursorPos(editor.cursorPos.l, editor.cursorPos.c + 1);
  } else if (e.keyCode == 40) { // downarrow  arrow 
    setCursorPos(editor.cursorPos.l + 1, editor.cursorPos.c);
  } else {
    var chr = String.fromCharCode(parseInt("0x" + e.keyIdentifier.substr(2)));

    if (!e.shiftKey) {
      chr = chr.toLowerCase(chr); 
    }
    
    spliceAtCursor(0, chr);
    setCursorPos(editor.cursorPos.l, editor.cursorPos.c + 1);
  } 

  reconstructEditorPre();
}

function handleClick(id) {
  var pos = eval("(" + id + ")");
  setCursorPos(pos.l, pos.c);
}

function reconstructEditorPre() {
  editorPre = document.getElementById("editorPre");
  source    = editor.source; 

  while(editorPre.hasChildNodes()) {
    editorPre.removeChild(editorPre.firstChild);
  }

  for(var l = 1; l <= numberOfLines(); l++) {
    var line = editor.source[l - 1];

    for(var c = 1; c <= line.length; c++) {
      var chr = line[c - 1];

      if(chr == '\n') {
        editorPre.innerHTML += "<span id=\"{l:" + l + ",c:" + c + "}\" onClick=\"handleClick(this.id)\"> </span>\n";
      } else {
        editorPre.innerHTML += "<span id=\"{l:" + l + ",c:" + c + "}\" onClick=\"handleClick(this.id)\">" + chr + "</span>";
      }
    }
  }

  cursorHighlighted = 1;
  blinkCursor();
}

function posToString(p) {
  return "{l:" + p.l + ",c:" + p.c + "}";
}

function blinkCursor() {
  cursorSpan = document.getElementById(posToString(editor.cursorPos));

  if(cursorHighlighted) {
    cursorSpan.style.backgroundColor = "white";
  } else {
    cursorSpan.style.backgroundColor = "gray";
  }
  
  cursorHighlighted = ! cursorHighlighted;
}
