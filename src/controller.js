var ws = null;


var lastBookmark = null; // FIXME: Move this into a closure, so as to avoid having a global variable.


function updateConnectButtons() {
  connectButton.disabled = ws != null;
  disconnectButton.disabled = ws == null;
}


function reconnect() {
  disconnect();
  ws = cesds.connect(connection.value);
  updateConnectButtons();
  ws.onopen = function() {cesds.requestModelsMetadata(ws, null, loadModels, reportError);};
}


function disconnect() {
  if (ws != null)
    cesds.disconnect(ws);
  ws = null;
  var root = document.getElementById("models");
  while (root.firstChild)
    root.removeChild(root.firstChild);
  updateConnectButtons();
}


var varTypes = {0 : "real", 1 : "integer", 2 : "string"};


function toggleDisplay(e) {
  return function () {e.style.display = e.style.display == "none" ? "block" : "none";};
}


function loadModels(result) {
  var root = document.getElementById("models");
  result.models.forEach(
    function(m) {
      var node = document.createElement("LI");
      var text = document.createTextNode(m.name);
      node.appendChild(text);
      node.style.cursor = "pointer"
      var subnode = document.createElement("DL");
      subnode.style.display = "none";
      node.addEventListener("click", toggleDisplay(subnode));
      text.addEventListener("click", toggleDisplay(subnode));
      subnode.addEventListener("click", function(e) {e.stopPropagation();});

      var detail = document.createElement("DT");
      text = document.createTextNode("ID");
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DD");
      text = document.createTextNode(m.id);
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DT");
      text = document.createTextNode("URI");
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DD");
      text = document.createTextNode(m.uri);
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DT");
      text = document.createTextNode("Variables");
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DD");
      var subdetail = document.createElement("OL");
      m.variables.forEach(
        function(v) {
          var item = document.createElement("LI");
          item.value = v.id
          u = v.units == "" ? "unitless" : v.units;
          text = document.createTextNode(v.name + " [" + u + "]: " + varTypes[v.type]);
          item.appendChild(text);
          subdetail.appendChild(item);
        }
      );
      detail.appendChild(subdetail);
      subnode.appendChild(detail);

      detail = document.createElement("DT");
      text = document.createTextNode("All Records");
      detail.appendChild(text);
      subnode.appendChild(detail);

      var showRecords =
        function(answer) {
          var data = document.getElementById("data");
          while (data.firstChild)
            data.removeChild(data.firstChild);
          var node = document.createElement("CAPTION");
          var span = document.createElement("SPAN");
          span.innerHTML = "Model ID:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
          span.style.fontWeight = "bold"
          node.appendChild(span);
          text = document.createTextNode(m.id);
          node.appendChild(text);
          node.appendChild(document.createElement("BR"));
          span = document.createElement("SPAN");
          span.innerHTML = ("Model Name:&nbsp;&nbsp;&nbsp;&nbsp;");
          span.style.fontWeight = "bold"
          node.appendChild(span);
          text = document.createTextNode(m.name);
          node.appendChild(text);
          node.appendChild(document.createElement("BR"));
          span = document.createElement("SPAN");
          span.innerHTML = ("Model URI:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
          span.style.fontWeight = "bold"
          node.appendChild(span);
          text = document.createTextNode(m.uri);
          node.appendChild(text);
          if (lastBookmark != null) {
            node.appendChild(document.createElement("BR"));
            span = document.createElement("SPAN");
            span.innerHTML = ("Bookmark ID&nbsp;&nbsp;&nbsp;&nbsp;");
            span.style.fontWeight = "bold"
            node.appendChild(span);
            text = document.createTextNode(lastBookmark.id);
            node.appendChild(text);
            node.appendChild(document.createElement("BR"));
            span = document.createElement("SPAN");
            span.innerHTML = ("Bookmark Name:&nbsp;");
            span.style.fontWeight = "bold"
            node.appendChild(span);
            text = document.createTextNode(lastBookmark.name);
            node.appendChild(text);
          }
          data.appendChild(node);
          node = document.createElement("THEAD");
          var row = document.createElement("TR");
          cell = document.createElement("TH");
          row.appendChild(cell);
          m.variables.forEach(
            function(v) {
              cell = document.createElement("TH");
              text = document.createTextNode(v.name);
              cell.appendChild(text);
              row.appendChild(cell);
            }
          );
          node.appendChild(row);
          data.appendChild(node);
          node = document.createElement("TBODY");
          answer.data.forEach(
            function (r) {
              row = document.createElement("TR");
              cell = document.createElement("TD");
              text = document.createTextNode(r.id);
              cell.appendChild(text);
              cell.style.textAlign = "right";
              row.appendChild(cell);
              m.variables.forEach(
                function(v) {
                  cell = document.createElement("TD");
                  text = document.createTextNode(r.values[v.id]);
                  cell.appendChild(text);
                  cell.style.textAlign = v.type < 2 ? "right" : "left";
                  row.appendChild(cell);
                }
              );
              node.appendChild(row);
            }
          );
          data.appendChild(node);
        };

      detail = document.createElement("DD");
      subdetail = document.createElement("BUTTON");
      text = document.createTextNode("show");
      subdetail.appendChild(text);
      subdetail.addEventListener("click", function() {lastBookmark = null; cesds.requestRecordsData(ws, m.id, null, null, null, showRecords, reportError);});
      detail.appendChild(subdetail);
      subnode.appendChild(detail);

      detail = document.createElement("DT");
      text = document.createTextNode("Bookmarks");
      detail.appendChild(text);
      subnode.appendChild(detail);

      detail = document.createElement("DD");
      subdetail = document.createElement("UL");
      var findBookmarks =
        function(answer) {
          answer.bookmarks.forEach(
            function (b) {
              var item = document.createElement("LI");
              text = document.createTextNode("[" + b.id + "] " + b.name);
              item.appendChild(text);
              item.addEventListener("click", function() {lastBookmark = b; cesds.requestRecordsData(ws, m.id, null, null, b.id, showRecords, reportError);});
              item.style.cursor = "pointer";
              subdetail.appendChild(item);
            }
          )
        };
      cesds.requestBookmarkMeta(ws, m.id, null, findBookmarks, reportError);
      detail.appendChild(subdetail);
      subnode.appendChild(detail);

      node.appendChild(subnode);
      root.appendChild(node);
    }
  );
}


function reportError(e) {
  alert(e);
}
