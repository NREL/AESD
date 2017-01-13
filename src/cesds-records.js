
var handlers = null;

function connect(wsURL) {
  var result = new WebSocket(wsURL);
  result.binaryType = "arraybuffer"
  handlers = new Object();
  result.onmessage =
    function(event) {
      var buffer = new Uint8Array(event.data);
      var response = proto.cesds.Response.deserializeBinary(buffer);
      var id = response.getId();
      if (id in handlers) {
        var answer = handlers[id](response);
        if (answer.done)
          delete handlers[id];
        if (answer.notify != null)
          // NB: Pass control to the user-supplied function only after all the internal bookkeeping is complete.
          answer.notify(answer.value);
      } else {
        console.warn("Unexpected response: ", response);
      }
    }
  return result;
}

function disconnect(connection) {
  connection.close();
}

function addHandler(id, f) {
  handlers[id] = f;
}

var ws = connect("ws://127.0.0.1:50374");


var version = 3;

var currentId = 0;

function nextId() {
  return optionalUInt32(++currentId);
}

function optionalInt32(x) {
  var result = new proto.cesds.OptionalInt32()
  result.setValue(x);
  return result;
}

function optionalUInt32(x) {
  var result = new proto.cesds.OptionalUInt32()
  result.setValue(x);
  return result;
}

function optionalString(x) {
  var result = new proto.cesds.OptionalString()
  result.setValue(x);
  return result;
}

function toVariable(v) {
  return {
           id    : v.getVarId()
         , name  : v.getVarName()
         , units : v.getUnits()
         , si    : v.getSiList()
         , scale : v.getScale()
         , type  : v.getType()
         };
}

function toModel(m) {
  return {
           id        : m.getModelId()
         , name      : m.getModelName()
         , uri       : m.getModelUri()
         , variables : m.getVariablesList().map(toVariable)
         };
}

function onResponse(response, handleError, handleModels, handleData, handleBookmarks) {
  if (handleError != null && response.hasError())
    return handleError(response.getError());
  else if (handleModels != null && response.hasModels())
    return handleModels(response.getChunkId(), response.getNextChunkId(), response.getModels());
  else if (handleData != null && response.hasData())
    return handleData(response.getChunkId(), response.getNextChunkId(), response.getData());
  else if (handleBookmarks != null && response.hasBookmarks())
    return handleBookmarks(response.getChunkId(), response.getNextChunkId(), response.getBookmarks());
  else {
    console.warn("Unhandled response: ", response);
    return {done : false, notify : handleError, value : null};
  }
}

function requestModelsMetadata(connection, modelId, notify, notifyError) {
  var request = new proto.cesds.Request();
  request.setVersion(version);
  request.setId(nextId());
  var m = new proto.cesds.RequestModelsMeta();
  if (modelId != null)
    m.setModelId(optionalString(modelId));
  request.setModelsMetadata(m);
  var result = {complete : false, models : []};
  var handleError =
    function(e) {
      console.warn("Error response: ", e);
      result.complete = false;
      return {done : true, notify : notifyError, value : e};
    };
  var handleModels =
    function(c, nc, ms) {
      result.models.push.apply(result.models, ms.getModelsList().map(toModel));
      result.complete = nc < 1;
      return {done : result.complete, notify : notify, value : result};
    };
  addHandler(request.getId(), response => onResponse(response, notifyError != null ? handleError : null, handleModels, null, null));
  connection.send(request.serializeBinary());
  return result;
}
