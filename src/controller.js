var ws = null;


var models = null;


function reconnect() {
  disconnect();
  ws = cesds.connect(connection.value);
  ws.onopen = function() {cesds.requestModelsMetadata(ws, null, loadModels, reportError);};
}


function disconnect() {
  if (ws != null)
    cesds.disconnect(ws);
  ws = null;
  var root = document.getElementById("models");
  while (root.firstChild)
    root.removeChild(root.firstChild);
}


var varTypes = {0 : "real", 1 : "integer", 2 : "string"};


function loadModels(result) {
  var root = document.getElementById("models");
  result.models.forEach(
    function(m) {
      var node = document.createElement("LI");
      var text = document.createTextNode(m.name);
      node.appendChild(text);
      var table = document.createElement("TABLE");
      var caption = document.createElement("CAPTION");
      text = document.createTextNode(m.uri);
      caption.appendChild(text);
      table.appendChild(caption);
      row = document.createElement("TR");
      cell = document.createElement("TH");
      text = document.createTextNode("ID");
      cell.appendChild(text);
      row.appendChild(cell);
      cell = document.createElement("TH");
      text = document.createTextNode("Name");
      cell.appendChild(text);
      row.appendChild(cell);
      cell = document.createElement("TH");
      text = document.createTextNode("Units");
      cell.appendChild(text);
      row.appendChild(cell);
      cell = document.createElement("TH");
      text = document.createTextNode("Type");
      cell.appendChild(text);
      row.appendChild(cell);
      table.appendChild(row);
      m.variables.forEach(
        function(v) {
          row = document.createElement("TR");
          cell = document.createElement("TD");
          text = document.createTextNode(v.id);
          cell.appendChild(text);
          row.appendChild(cell);
          cell = document.createElement("TD");
          text = document.createTextNode(v.name);
          cell.appendChild(text);
          row.appendChild(cell);
          cell = document.createElement("TD");
          text = document.createTextNode(v.units);
          cell.appendChild(text);
          row.appendChild(cell);
          cell = document.createElement("TD");
          text = document.createTextNode(varTypes[v.type]);
          cell.appendChild(text);
          row.appendChild(cell);
          table.appendChild(row);
        }
      );
      node.appendChild(table);
      root.appendChild(node);
    }
  );
}


function reportError(e) {
  alert(e);
}
