function inRange(p, r) {
  return (p.l > r.fromLine || (p.l == r.fromLine && p.c >= r.fromCol))
      && (p.l < r.toLine   || (p.l == r.toLine   && p.c <= r.toCol));
}

function highlight(r) {
  var elems = document.getElementsByTagName("span");
  for (var i = 0; i < elems.length; i++) {
    var elem = elems[i];
    var p    = eval("(" + elem.id + ")");

    if(inRange(p, r)) {
      elem.style.background = "yellow";
    } else {
      elem.style.background = "white";
    }
  }
}

function handleClick(id) {
  p = eval("(" + id + ")");

  for(var i = 0; i < globalIdMap.length; i++) {
    if(inRange(p, globalIdMap[i].src)) {
      highlight(globalIdMap[i].def);
      break;
    }
  }
}

function updateSource() {
  console.log("Updating..");

  source    = document.getElementById("source").value;
  sourcePre = document.getElementById("sourcePre");
  errorsPre = document.getElementById("errorsPre");
 
  // Clear previous results
  while(sourcePre.hasChildNodes()) {
    sourcePre.removeChild(sourcePre.firstChild);
  }
  while(errorsPre.hasChildNodes()) {
    errorsPre.removeChild(errorsPre.firstChild);
  }

  // Create <pre> with the source, with each character tagged with a <span>
  // indicating its line and column number
  var line   = 1;
  var column = 1;
  for(var i = 0; i < source.length; i++) {
    if(source.charAt(i) == '\n') {
      sourcePre.innerHTML += "\n";
      line++;
      column = 1;
    } else {
      sourcePre.innerHTML += "<span id=\"{l:" + line + ",c:" + column + "}\" onClick=\"handleClick(this.id)\">" + source.charAt(i) + "</span>";
      column++;
    }
  }

  // Send request to the IDE server
  console.log("Sending request..");
  var xmlHttp = new XMLHttpRequest();
  xmlHttp.open("POST", "http://localhost:8080", true);
  xmlHttp.send(source);
  xmlHttp.onreadystatechange = processIdeResponse;
  
  // Don't actually submit the form
  return false; 
}

function processIdeResponse(progress) {
  var xmlHttp = progress.currentTarget;
  
  if(xmlHttp.readyState != 4) {
    return;
  }

  if(xmlHttp.status != 200) {
    errorsPre.innerHTML = "<b>ERROR: Could not contact server</b>";
    return;
  }

  var ideResponse = eval("(" + xmlHttp.responseText + ")");
  console.log(ideResponse);

  // Output errors
  errorsPre.innerHTML = "Errors: " + JSON.stringify(ideResponse.errors);

  // Construct globalIdMap 
  var idMap = ideResponse.idMap;
  globalIdMap = [];
  for(var i = 0; i < idMap.length; i++) {
    var src    = idMap[i][0];
    var idInfo = idMap[i][1];

    if(typeof idInfo.idDefSpan.Left !== 'undefined') {
      var def    = idInfo.idDefSpan.Left;
      
      var srcSpan = { fromLine: src[1][0]
                    , fromCol:  src[1][1]
                    , toLine:   src[2][0]
                    , toCol:    src[2][1] 
                    }

      var defSpan = { fromLine: def[1][0]
                    , fromCol:  def[1][1]
                    , toLine:   def[2][0]
                    , toCol:    def[2][1] 
                    }

      globalIdMap.push({src: srcSpan, def: defSpan});
    }
  }
}
