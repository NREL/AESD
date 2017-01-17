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
  if (!result.complete)
    return;
  var first = true;
  var root = document.getElementById("models");
  result.models.forEach(
    function(m) {
      var node = document.createElement("LI");
      var text = document.createTextNode(m.name);
      node.appendChild(text);
      node.style.cursor = "pointer"
      var subnode = document.createElement("DL");
      if (first)
        first = false;
      else
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
          if (!answer.complete)
            return;
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
          plot(m.variables, answer.data);
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
          if (!answer.complete)
            return;
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


/* MODIFIED VERSION OF <https://bl.ocks.org/jasondavies/1341281> */


function plot(variables, records) {


  var root = document.getElementById("plot");
  while (root.firstChild)
    root.removeChild(root.firstChild);

  var nNumerics = 0;
  var numerics = new Object();
  variables.filter(variable => variable.type < 2).forEach(function (v) {++nNumerics; numerics[v.id] = v.name;});
  
  if (showPlot.checked && records.length > 2 && nNumerics > 2) {
    document.getElementById("data").style.top = "500px"
  } else {
    document.getElementById("data").style.top = "0px"
    return;
  }

  numericRecords =
    records.map(
      function(r) {
        var x = {};
        for (var i in numerics)
          x[numerics[i]] = r.values[i];
        return x;
      }
    );

  var margin = {top: 30, right: 10, bottom: 10, left: 10},
      width = root.width.animVal.value - margin.left - margin.right,
      height = root.height.animVal.value - margin.top - margin.bottom;
  
  var x = d3.scale.ordinal().rangePoints([0, width], 1),
      y = {},
      dragging = {};
  
  var line = d3.svg.line(),
      axis = d3.svg.axis().orient("left"),
      background,
      foreground;
  
  function position(d) {
    var v = dragging[d];
    return v == null ? x(d) : v;
  }
  
  function transition(g) {
    return g.transition().duration(500);
  }
  
  // Returns the path for a given data point.
  function path(d) {
    return line(dimensions.map(function(p) { return [position(p), y[p](d[p])]; }));
  }
  
  function brushstart() {
    d3.event.sourceEvent.stopPropagation();
  }
  
  // Handles a brush event, toggling the display of foreground lines.
  function brush() {
    var actives = dimensions.filter(function(p) { return !y[p].brush.empty(); }),
        extents = actives.map(function(p) { return y[p].brush.extent(); });
    foreground.style("display", function(d) {
      return actives.every(function(p, i) {
        return extents[i][0] <= d[p] && d[p] <= extents[i][1];
      }) ? null : "none";
    });
  }
  
  {
  
    var svg = d3.select("#plot")
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
    // Extract the list of dimensions and create a scale for each.
    x.domain(dimensions = d3.keys(numericRecords[0]).filter(function(d) {
      return d != "name" && (y[d] = d3.scale.linear()
          .domain(d3.extent(numericRecords, function(p) { return +p[d]; }))
          .range([height, 0]));
    }));
  
    // Add grey background lines for context.
    background = svg.append("g")
        .attr("class", "background")
      .selectAll("path")
        .data(numericRecords)
      .enter().append("path")
        .attr("d", path);
  
    // Add blue foreground lines for focus.
    foreground = svg.append("g")
        .attr("class", "foreground")
      .selectAll("path")
        .data(numericRecords)
      .enter().append("path")
        .attr("d", path);
  
    // Add a group element for each dimension.
    var g = svg.selectAll(".dimension")
        .data(dimensions)
      .enter().append("g")
        .attr("class", "dimension")
        .attr("transform", function(d) { return "translate(" + x(d) + ")"; })
        .call(d3.behavior.drag()
          .origin(function(d) { return {x: x(d)}; })
          .on("dragstart", function(d) {
            dragging[d] = x(d);
            background.attr("visibility", "hidden");
          })
          .on("drag", function(d) {
            dragging[d] = Math.min(width, Math.max(0, d3.event.x));
            foreground.attr("d", path);
            dimensions.sort(function(a, b) { return position(a) - position(b); });
            x.domain(dimensions);
            g.attr("transform", function(d) { return "translate(" + position(d) + ")"; })
          })
          .on("dragend", function(d) {
            delete dragging[d];
            transition(d3.select(this)).attr("transform", "translate(" + x(d) + ")");
            transition(foreground).attr("d", path);
            background
                .attr("d", path)
              .transition()
                .delay(500)
                .duration(0)
                .attr("visibility", null);
          }));
  
    // Add an axis and title.
    g.append("g")
        .attr("class", "axis")
        .each(function(d) { d3.select(this).call(axis.scale(y[d])); })
      .append("text")
        .style("text-anchor", "middle")
        .attr("y", -9)
        .text(function(d) { return d; });
  
    // Add and store a brush for each axis.
    g.append("g")
        .attr("class", "brush")
        .each(function(d) {
          d3.select(this).call(y[d].brush = d3.svg.brush().y(y[d]).on("brushstart", brushstart).on("brush", brush));
        })
      .selectAll("rect")
        .attr("x", -8)
        .attr("width", 16);

  }


}
