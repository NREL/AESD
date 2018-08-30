var debug = false;


var handlers = null;


function connect(wsURL) {
  var result = new WebSocket(wsURL);
  result.binaryType = "arraybuffer"
  handlers = new Object();
  result.onmessage =
    function(event) {
      var buffer = new Uint8Array(event.data);
      var response = proto.RecordsAPI.Response.deserializeBinary(buffer);
      var id = response.getId();
      if (debug) {
        console.log("Response ID: " , id.getValue());
        console.log("  Chunk:     " , response.getChunkId());
        console.log("  Next chunk: ", response.getNextChunkId());
      }
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
  connection.close(1000, "normal termination");
}


function addHandler(id, f) {
  if (debug)
    console.log("Request ID: ", id.getValue());
  handlers[id] = f;
}


var version = 4;


var currentId = 0;


function nextId() {
  return optionalUInt32(++currentId);
}


function optionalInt32(x) {
  var result = new proto.RecordsAPI.OptionalInt32()
  result.setValue(x);
  return result;
}


function optionalUInt32(x) {
  var result = new proto.RecordsAPI.OptionalUInt32()
  result.setValue(x);
  return result;
}


function optionalString(x) {
  var result = new proto.RecordsAPI.OptionalString()
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
  var request = new proto.RecordsAPI.Request();
  request.setVersion(version);
  request.setId(nextId());
  var m = new proto.RecordsAPI.RequestModelsMeta();
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


function fromValue(v) {
  if (v.hasRealValue())
    return v.getRealValue();
  else if (v.hasIntegerValue())
    return v.getIntegerValue();
  else if (v.hasStringValue())
    return v.getStringValue();
  else
    return null;
}


function fromRecord(r) {
  var values = new Object();
  r.getVariablesList().forEach(vv => values[vv.getVarId()] = fromValue(vv.getValue()));
  return {
           id     : r.getRecordId()
         , values : values
         };
}


function fromRecordList(rl) {
  return rl.getRecordsList().map(fromRecord);
}


function fromRecordTable(rt) {
  var vids = rt.getVarIdsList();
  var nv = vids.length;
  var rids = rt.getRecIdsList();
  var nr = rids.length;
  var vals = null;
  if (rt.hasReals())
    vals = rt.getReals().getValuesList();
  else if (rt.hasIntegers())
    vals = rt.getIntegers().getValuesList();
  else if (rt.hasStrings())
    vals = rt.getStrings().getValuesList();
  else
    vals = Array(nv * nr);
  var rows = [];
  for (var i = 0; i < nr; ++i) {
    var obj = new Object();
    for (var j = 0; j < nv; ++j)
      obj[vids[j]] = vals[i * nv + j];
    rows.push({id : rids[i], values : obj});
  }
  return rows;
}


function fromRecordData(rd) {
  if (rd.hasList())
    return fromRecordList(rd.getList());
  else if (rd.hasTable())
    return fromRecordTable(rd.getTable());
  else
    console.error("Invalid record data: ", rd);
}


function requestRecordsData(connection, modelId, maxRecords, variableIds, bookmarkId, notify, notifyError) {
  var request = new proto.RecordsAPI.Request();
  request.setVersion(version);
  request.setId(nextId());
  var d = new proto.RecordsAPI.RequestRecordsData();
  d.setModelId(modelId);
  if (maxRecords != null)
    d.setMaxRecords(maxRecords);
  if (variableIds != null)
    d.setVariableIdsList(variableIds);
  if (bookmarkId != null)
    d.setBookmarkId(bookmarkId);
  request.setRecordsData(d);
  var result = {complete : false, data : []};
  var handleError =
    function(e) {
      console.warn("Error response: ", e);
      result.complete = false;
      return {done : true, notify : notifyError, value : e};
    };
  var handleData =
    function(c, nc, rd) {
      result.data.push.apply(result.data, fromRecordData(rd));
      result.complete = nc < 1;
      return {done : result.complete, notify : notify, value : result};
    };
  addHandler(request.getId(), response => onResponse(response, notifyError != null ? handleError : null, null, handleData, null));
  connection.send(request.serializeBinary());
  return result;
}


function fromInterval(i) {
  var result = new Object();
  result.first = i.getFirstRecord();
  result.last = i.getLastRecord();
  return result;
}


function fromBookmark(b) {
  var result =  {
                  id   : b.getBookmarkId()
                , name : b.getBookmarkName()
                };
  if (b.hasInterval())
    result.interval = fromInterval(b.getInterval());
  else if (b.hasSet())
    result.set = b.getSet().getRecordIdsList();
  return result;
}


function fromBookmarks(bs) {
  return bs.getBookmarkMetasList().map(fromBookmark);
}


function requestBookmarkMeta(connection, modelId, bookmarkId, notify, notifyError) {
  var request = new proto.RecordsAPI.Request();
  request.setVersion(version);
  request.setId(nextId());
  var b = new proto.RecordsAPI.RequestBookmarkMeta();
  b.setModelId(modelId);
  if (bookmarkId != null)
    b.setBookmarkId(string(bookmarkId));
  request.setBookmarkMeta(b);
  var result = {complete : false, bookmarks : []};
  var handleError =
    function(e) {
      console.warn("Error response: ", e);
      result.complete = false;
      return {done : true, notify : notifyError, value : e};
    };
  var handleBookmarks =
    function (c, nc, bs) {
      result.bookmarks.push.apply(result.bookmarks, fromBookmarks(bs));
      result.complete = nc < 1;
      return {done : result.complete, notify : notify, value : result};
    };
  addHandler(request.getId(), response => onResponse(response, notifyError != null ? handleError : null, null, null, handleBookmarks));
  connection.send(request.serializeBinary());
  return result;
}


function requestSaveBookmarkInterval(connection, modelId, name, firstRecord, lastRecord, notify, notifyError) {
  var i = new proto.RecordsAPI.BookmarkIntervalContent();
  i.setFirstRecord(firstRecord);
  i.setLastRecord(lastRecord);
  return requestSaveBookmark(connection, modelId, name, b => {b.setInterval(i); return b;}, notify, notifyError);
}


function requestSaveBookmarkSet(connection, modelId, name, records, notify, notifyError) {
  var s = new proto.RecordsAPI.BookmarkSetContent();
  s.setRecordIdsList(records != null ? records : []);
  return requestSaveBookmark(connection, modelId, name, b => {b.setSet(s); return b}, notify, notifyError);
}


function requestSaveBookmark(connection, modelId, name, f, notify, notifyError) {
  var request = new proto.RecordsAPI.Request();
  request.setVersion(version);
  request.setId(nextId());
  var s = new proto.RecordsAPI.RequestSaveBookmark();
  s.setModelId(modelId);
  var b = new proto.RecordsAPI.BookmarkMeta();
  b.setBookmarkName(name);
  s.setNewBookmark(f(b));
  request.setSaveBookmark(s);
  var result = {complete : false, bookmark : null};
  var handleError =
    function(e) {
      console.warn("Error response: ", e);
      result.complete = false;
      return {done : true, notify : notifyError, value : e};
    };
  var handleBookmark =
    function(c, nc, bs) {
      result.bookmark = fromBookmark(bs.getBookmarkMetasList()[0]);
      result.complete = nc < 1;
      return {done : true, notify : notify, value : result};
    };
  addHandler(request.getId(), response => onResponse(response, notifyError != null ? handleError : null, null, null, handleBookmark));
  connection.send(request.serializeBinary());
  return result;
}


module.exports = {
                   connect                    : connect
                 , disconnect                 : disconnect
                 , requestModelsMetadata      : requestModelsMetadata
                 , requestRecordsData         : requestRecordsData
                 , requestBookmarkMeta        : requestBookmarkMeta
                 , requestSaveBookmarkInterval: requestSaveBookmarkInterval
                 , requestSaveBookmarkSet     : requestSaveBookmarkSet
                 };
