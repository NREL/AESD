var ws = null;


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
  return function () {e.style.display = e.style.display == "none" ? "block" : "none";}
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
      var detail = document.createElement("DT");
      text = document.createTextNode("ID");
      detail.appendChild(text);
      subnode.appendChild(detail);
      var detail = document.createElement("DD");
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
      node.appendChild(subnode);
      root.appendChild(node);
    }
  );
}


function reportError(e) {
  alert(e);
}
