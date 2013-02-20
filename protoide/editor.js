var cursorHighlighted = 1;
var editor = { 
    cursorPos: {l:1, c:1}
  , cursorIx:  0
  , source:    "\n"
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
  var line   = 1;
  var column = 1;
  var source = editor.source;
  
  for(var i = 0; i < source.length; i++) {
    if(source[i] == '\n') {
      if(line == l) {
        return column;
      } else {
        line++;
        column = 1;
      }
    } else {
      column++;
    }
  }

  throw "Invalid line";
}

function setCursorPos(l, c) {
  var source = editor.source;
  var line   = 1;
  var column = 1;
  var ix     = 0;

  for(var i = 0; i < source.length; i++) {
    if(line == l && column == c) {
      editor.cursorPos = {l: line, c: column};
      editor.cursorIx  = ix;
      return;
    }

    if(source[i] == '\n') {
      if(line == l || (line + 1 == l && c == 0)) {
        editor.cursorPos = {l: line, c: column};
        editor.cursorIx  = ix;
        return;
      }

      line++;
      column = 1;
    } else {
      column++;
    }

    ix++;
  }
  
  editor.cursorPos = {l: line, c: column};
  editor.cursorIx  = ix;
}

function numberOfLines() {
  // TODO: cache?
  var numLines = 0;
  var source   = editor.source;
  for(var i = 0; i < source.length; i++) {
    if(source[i] == '\n') {
      numLines++;
    }
  }
  return numLines;
}

function editorOnKeyUp(e) {
  var chr;

  if(e.keyCode == 13) { // enter
    editor.source = editor.source.splice(editor.cursorIx, 0, "\n");
    setCursorPos(editor.cursorPos.l + 1, 1);
  } else if (e.keyCode == 8) { // backspace {
    setCursorPos(editor.cursorPos.l, editor.cursorPos.c - 1);
    editor.source = editor.source.splice(editor.cursorIx, 1, ""); 
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
    
    editor.source = editor.source.splice(editor.cursorIx, 0, chr);
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

  var line   = 1;
  var column = 1;
  for(var i = 0; i < source.length; i++) {
    if(source.charAt(i) == '\n') {
      editorPre.innerHTML += "<span id=\"{l:" + line + ",c:" + column + "}\" onClick=\"handleClick(this.id)\"> </span>\n";
      line++;
      column = 1;
    } else {
      editorPre.innerHTML += "<span id=\"{l:" + line + ",c:" + column + "}\" onClick=\"handleClick(this.id)\">" + source.charAt(i) + "</span>";
      column++;
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
