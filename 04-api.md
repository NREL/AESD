
# Records API, Version 4

The Records API consists of Google Protobuf 3 [@protobuf] messages used for requesting and providing data and metadata for record-oriented information. This section contains the complete specification for version 4 of the Records API. Clients send `Request` messages and servers send `Response` messages, typically transported via WebSockets [@websockets].

## Message Groups

The message types in the Records API are organized into thematic groups below.

### Requests and Responses

`Request` messages are sent from client to server and `Response` messages are sent from server to client.  Request messages contain a specific type of request and response messages contain a corresponding specific type of response.

*   [Request](#AesdRecords.Request)
*   [RequestModelsMeta](#AesdRecords.RequestModelsMeta)
*   [RequestRecordsData](#AesdRecords.RequestRecordsData)
*   [RequestWork](#AesdRecords.RequestWork)
*   [RequestBoomarkMeta](#AesdRecords.RequestBoomarkMeta)
*   [RequestSaveBookmark](#AesdRecords.RequestSaveBookmark)
*   [RequestCancel](#AesdRecords.RequestCancel)
*   [Response](#AesdRecords.Response)

### Metadata

Metadata messages describe data sources ("models") and variables.

*   [ModelMeta](#AesdRecords.ModelMeta)
*   [ModelMetaList](#AesdRecords.ModelMetaList)
*   [DomainMeta](#AesdRecords.DomainMeta)
*   [VarMeta](#AesdRecords.VarMeta)
*   [VariableType](#AesdRecords.VariableType)
*   [VarSet](#AesdRecords.VarSet)
*   [VarInterval](#AesdRecords.VarInterval)

### Data Records

Data is represented as either lists of records or tables of them.

*   [Record](#AesdRecords.Record)
*   [VarValue](#AesdRecords.VarValue)
*   [Value](#AesdRecords.Value)
*   [RecordData](#AesdRecords.RecordData)
*   [RecordList](#AesdRecords.RecordList)
*   [RecordTable](#AesdRecords.RecordTable)

### Filtering

Records can be filtered by logical operations on conditions on values of variables in the records.

*   [FilterExpression](#AesdRecords.FilterExpression)
*   [FilterNot](#AesdRecords.FilterNot)
*   [FilterIntersection](#AesdRecords.FilterIntersection)
*   [FilterUnion](#AesdRecords.FilterUnion)
*   [DomainMeta](#AesdRecords.DomainMeta)

### Bookmarks

Bookmarks record particular sets or records or conditions on record data.

*   [BookmarkMeta](#AesdRecords.BookmarkMeta)
*   [BookmarkMetaList](#AesdRecords.BookmarkMetaList)
*   [BookmarkIntervalContent](#AesdRecords.BookmarkIntervalContent)
*   [BookmarkSetContent](#AesdRecords.BookmarkSetContent)

### Miscellaneous

The following messages wrap data types for the content of records.

*   [DoubleList](#AesdRecords.DoubleList)
*   [IntegerList](#AesdRecords.IntegerList)
*   [StringList](#AesdRecords.StringList)
*   [OptionalInt32](#AesdRecords.OptionalInt32)
*   [OptionalUInt32](#AesdRecords.OptionalUInt32)
*   [OptionalString](#AesdRecords.OptionalString)

## General conventions

All fields are technically optional in ProtoBuf 3, but some fields may be required in each message type in order for the message to be semantically valid.  In the following specifications for the messages, fields are annotated as *semantically required* or *semantically optional*.  Also, the specification notes when field in the [protobuf `oneof` construct](https://developers.google.com/protocol-buffers/docs/proto3#oneof) are required or mutually exclusive.

Furthermore, one cannot determine whether an optional value has been set or not if it is just a value, as opposed to a message. That is not true for fields that are messages, where the absence of the field truly indicates that the value is absent, not just a default or unset value. The message `OptionalString`, for example, is used in this API to indicate whether a character string value is truly present. Thus [`RequestModelsMeta`](#AesdRecords.RequestModelsMeta) has a `model_id` field that indicates whether the request is for all models, when the field has not been set, or for a specific one, when the field has been set.

Throughout this specification, the following types are used for identifiers:
*  `var_id` is [int32](#int32)
*  `model_id` is [string](#string)
*  `record_id` is [int64](#int64)

This specification conforms to [Protocol Buffers version 3](https://developers.google.com/protocol-buffers/docs/proto3).

## Messages


### BookmarkIntervalContent {#AesdRecords.BookmarkIntervalContent}

A range of [record identifiers](#AesdRecords.Record) can specify the content of a [bookmark](#AesdRecords.BookmarkMeta). Bookmark interval content provides a convenient means to bookmark a contiguous selection of records in a [model](#AesdRecords.ModelMeta).

Both fields in this message are optional:

*   If neither field is present, then the bookmark interval designates all records in the model.
*   If only `first_record`is present, then the bookmark interval designates all records starting from that record identifier.
*   If only `last_record` is present, then the bookmark interval designates all records ending at that record identifier. For a dynamic model, such a bookmark interval includes all "future" records.
*   If both fields are present, then the bookmark interval designates all records between the two identifiers, inclusive.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| first_record | [int64](#int64) | optional | [semantically optional] The identifier for the first record in the interval. |
| last_record | [int64](#int64) | optional | [semantically optional] The identifier for the last record in the interval. |



### BookmarkMeta {#AesdRecords.BookmarkMeta}

A bookmark is metadata defining a subset of records in a [model](#AesdRecords.ModelMeta).

There are three alternatives to specifying a bookmark:

1.  [Interval content](#AesdRecords.BookmarkIntervalContent) specifies a range of records in the bookmark.
2.  [Set content](#AesdRecords.BookmarkSetContent) specifies a list of records in the bookmark.
3.  A [filter expression](#AesdRecords.FilterExpression) defines a set of logical conditions for determining whether a record is in the bookmark.

*Exactly one of `interval`, `set`, or `filter` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_id | [string](#string) | optional | [semantically optional] When creating a new bookmark, this field must be empty: the server will create a unique identifier for the bookmark. This identifier uniquely identifies the bookmark *on the particular server*. |
| bookmark_name | [string](#string) | optional | [semantically required] A name for the bookmark, useful for displaying the bookmark to users. This need not be unique, although it is recommended to be so. |
| interval | [BookmarkIntervalContent](#AesdRecords.BookmarkIntervalContent) | optional | The range of records in the bookmark. |
| set | [BookmarkSetContent](#AesdRecords.BookmarkSetContent) | optional | The list of records in the bookmark. |
| filter | [FilterExpression](#AesdRecords.FilterExpression) | optional | Logical conditions for defining which records are in the bookmark. |



### BookmarkMetaList {#AesdRecords.BookmarkMetaList}

Bookmarks may be grouped into lists (sets).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_metas | [BookmarkMeta](#AesdRecords.BookmarkMeta) | repeated | [semantically optional] The bookmarks in the list. |



### BookmarkSetContent {#AesdRecords.BookmarkSetContent}

A list (set) of [record identifiers](#AesdRecords.Record) can specify the contents of a [bookmark](#AesdRecords.BookmarkMeta). Bookmark set content provides a convenient means to bookmark a specific selection of non-continuous records in a [model](#AesdRecords.ModelMeta).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_ids | [int64](#int64) | repeated | [semantically optional] The list of record identifiers in the set. |



### DomainMeta {#AesdRecords.DomainMeta}

The domain (set of valid values) for a variable.

There are two alternatives to specifying a domain:

1.  [An interval](#AesdRecords.VarInterval) specifies a range of values in the domain.
2.  [A set](#AesdRecords.VarSet) specifies a list of values in the domain.

*Exactly one of `interval` or `set` must be specified in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] |
| interval | [VarInterval](#AesdRecords.VarInterval) | optional | The interval of values in the domain. |
| set | [VarSet](#AesdRecords.VarSet) | optional | The list of values in the domain. |



### DoubleList {#AesdRecords.DoubleList}

A list of real numbers.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [double](#double) | repeated | [semantically required] The real numbers. |



### FilterExpression {#AesdRecords.FilterExpression}

A filtering expression is a composition of logical conditions on a [record](#AesdRecords.Record). It can be used to filter records.
There are four alternatives to specifying a filter expression:

1.  The [logical negation](#AesdRecords.FilterNot) of another filtering expression.
2.  The [set union](#AesdRecords.FilterUnion) of multiple filtering expressions.
3.  The [set intersection](#AesdRecords.FilterIntersection) of multiple filtering expressions.
4.  [Particular values](#AesdRecords.DomainMeta) of variables in a record.

*Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_not | [FilterNot](#AesdRecords.FilterNot) | optional | Logical negation of an expression. |
| filter_union | [FilterUnion](#AesdRecords.FilterUnion) | optional | Set union of expressions. |
| filter_intersection | [FilterIntersection](#AesdRecords.FilterIntersection) | optional | Set intersection of expressions. |
| filter_domain | [DomainMeta](#AesdRecords.DomainMeta) | optional | Particular values of variables. |



### FilterIntersection {#AesdRecords.FilterIntersection}

Set intersection of filtering expressions. A record satisfies this expression if it satisfies all of `filter_expressions`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expressions | [FilterExpression](#AesdRecords.FilterExpression) | repeated | [semantically required] The expressions to be intersected. |



### FilterNot {#AesdRecords.FilterNot}

Logically negate a filtering expression. A record satisfies this expression if it does not satisfy `filter_expression`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expression | [FilterExpression](#AesdRecords.FilterExpression) | optional | [semantically required] The expression to be negated. |



### FilterUnion {#AesdRecords.FilterUnion}

Set union of filtering expressions. A record satisfies this expression if it satisfies any of `filter_expressions`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expressions | [FilterExpression](#AesdRecords.FilterExpression) | repeated | [semantically required] The expressions to be unioned. |



### IntegerList {#AesdRecords.IntegerList}

A list of integers.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [sint64](#sint64) | repeated | [semantically required] The integers. |



### ModelMeta {#AesdRecords.ModelMeta}

Metadata for a model.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The unique identifier for the model *on the particular server*. |
| model_name | [string](#string) | optional | [semantically required] A name for the model, useful for display the model to users. This need not be unique, although it is recommended to be so. |
| model_uri | [string](#string) | optional | [semantically required] The unique URI for the model. Additional metadata may be obtained by dereferencing that URI. |
| variables | [VarMeta](#AesdRecords.VarMeta) | repeated | [semantically required] Metadata for the variables. |
| inputs | [DomainMeta](#AesdRecords.DomainMeta) | repeated | [semantically optional] Metadata for input values to the model, if any. |



### ModelMetaList {#AesdRecords.ModelMetaList}

A list of metadata for models.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| models | [ModelMeta](#AesdRecords.ModelMeta) | repeated | [semantically optional] The metadata for the models. |



### OptionalInt32 {#AesdRecords.OptionalInt32}

Wrapper for an optional signed integer.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [int32](#int32) | optional | [semantically required] The signed integer value. |



### OptionalString {#AesdRecords.OptionalString}

Wrapper for an optional string.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [string](#string) | optional | [semantically required] The character string value. |



### OptionalUInt32 {#AesdRecords.OptionalUInt32}

Wrapper for an optional unsigned integer.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [uint32](#uint32) | optional | [semantically required] The unsigned integer value. |



### Record {#AesdRecords.Record}

A record is a list of variables and their associated values.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_id | [int64](#int64) | optional | [semantically required] A unique identifier for the record. |
| variables | [VarValue](#AesdRecords.VarValue) | repeated | [semantically optional] The values for variables in the record. |



### RecordData {#AesdRecords.RecordData}

A collection or records.

There are two alternatives to specifying record data:

1.  [A list](#AesdRecords.RecordList) specifies a heterogeneously typed list.
2.  [A table](#AesdRecords.RecordTable) specifies a homogeneously typed table.

 *Exactly one of `list` or `table` must be present in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| list | [RecordList](#AesdRecords.RecordList) | optional | A heterogeneously typed list of records. |
| table | [RecordTable](#AesdRecords.RecordTable) | optional | A homogeneously typed table of records. |



### RecordList {#AesdRecords.RecordList}

A list of records. The list is heterogeneous in the sense that each variable may have a different type.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| records | [Record](#AesdRecords.Record) | repeated | [semantically optional] The list of records. |



### RecordTable {#AesdRecords.RecordTable}

A homogeneously typed table of records, where each variable has each type, with a row for each record and a column for each variable.

 This message represents the following table:

 | Record Identifier | `var_id[0]`  | `var_id[1]`  | . . . | `var_id[N]`  |
 |-------------------|--------------|--------------|-------|--------------|
 | `rec_id[0]`       | `list[0][0]` | `list[0][1]` | . . . | `list[0][N]` |
 | `rec_id[1]`       | `list[1][0]` | `list[1][1]` | . . . | `list[1][N]` |
 | . . .             | . . .        | . . .        | . . . | . . .        |
 | `rec_id[M]`       | `list[M][0]` | `list[M][1]` | . . . | `list[M][N]` |

 The underlying list is a **single** array, addressable using the following [row-major index formula](https://en.wikipedia.org/wiki/Row-_and_column-major_order)
 	list[row][var] = array[var + NY * row]
 where `NX` = length of `rec_ids` and `NY` = length of `var_ids`.

*Exacly one of `reals`, `integers`, or `strings` must be specified in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_ids | [int32](#int32) | repeated | [semantically required] The identifiers of the variables (columns) in the table. |
| rec_ids | [int64](#int64) | repeated | [semantically required] The identifiers of the records (rows) in the table. |
| reals | [DoubleList](#AesdRecords.DoubleList) | optional | The real numbers comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |
| integers | [IntegerList](#AesdRecords.IntegerList) | optional | The integers comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |
| strings | [StringList](#AesdRecords.StringList) | optional | The character strings comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |



### Request {#AesdRecords.Request}

A request. There are six types of requests:

| Request                        | Response                                          |
|--------------------------------|---------------------------------------------------|
| Metadata for model(s)          | [ModelMetaList](#AesdRecords.ModelMetaList)       |
| Data records                   | [RecordData](#AesdRecords.RecordData)             |
| Metadata for bookmark(s)       | [BookmarkMetaList](#AesdRecords.BookmarkMetaList) |
| Saving a bookmark              | [BookmarkMetaList](#AesdRecords.BookmarkMetaList) |
| Canceling a previous request   | n/a                                               |
| New work, such as a simulation | [RecordData](#AesdRecords.RecordData)             |

*Exactly one of `models_metadata`, `records_data`, `bookmark_meta`, `save_bookmark`, `cancel`, or `work` must be specified in the message.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional | [semantically required] The version number for the API. *This must be the number **four**.* |
| id | [OptionalUInt32](#AesdRecords.OptionalUInt32) | optional | [semantically optional, but recommended] An identifier which will be used to tag responses, so that responses can be correlated with requests. |
| subscribe | [bool](#bool) | optional | [semantically optional] Whether to continue receiving responses indefinitely, as new records become available. This is useful, for example, when a sensor is reporting measurements periodically or when simulations are reporting a series or results. Use [RequestCancel](#AesdRecords.RequestCancel) to end the subscription. |
| models_metadata | [RequestModelsMeta](#AesdRecords.RequestModelsMeta) | optional | Request metadata for model(s). |
| records_data | [RequestRecordsData](#AesdRecords.RequestRecordsData) | optional | Request data records. |
| bookmark_meta | [RequestBookmarkMeta](#AesdRecords.RequestBookmarkMeta) | optional | Request metadata for bookmark(s). |
| save_bookmark | [RequestSaveBookmark](#AesdRecords.RequestSaveBookmark) | optional | Request save a new bookmark or update an existing one. |
| cancel | [RequestCancel](#AesdRecords.RequestCancel) | optional | Request cancel a previous request). |
| work | [RequestWork](#AesdRecords.RequestWork) | optional | Request request work (e.g., simulation results). |



### RequestBookmarkMeta {#AesdRecords.RequestBookmarkMeta}

A request for one or more bookmarks for a [model](#AesdRecords.ModelMeta).

The response to this request is [BookmarkMetaList](#AesdRecords.MetaList)

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to list bookmarks. |
| bookmark_id | [OptionalString](#AesdRecords.OptionalString) | optional | [semantically optional] If empty, list all bookmarks for the model. Otherwise, list just the bookmark metadata for this specific bookmark identifier. |



### RequestCancel {#AesdRecords.RequestCancel}

Cancel a previous request.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| id | [OptionalUInt32](#AesdRecords.OptionalUInt32) | optional | [semantically required] Which request to cancel. |



### RequestModelsMeta {#AesdRecords.RequestModelsMeta}

A request for metadata about model(s).

The response to this request is [ModelMetaList](#AesdRecords.ModelMetaList).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [OptionalString](#AesdRecords.OptionalString) | optional | [semantically optional] If absent, the request is for metadata for all models. Otherwise the request is for the specifically identified model. |



### RequestRecordsData {#AesdRecords.RequestRecordsData}

Request record data for a model.

There are three alternatives to requesting record data.

1.  Request all records.
2.  Request records in [a bookmark](#AesdRecords.BookmarkMeta).
3.  [Filter](#AesdRecords.FilterExpression) records according to a criterion.

The response to this request is [RecordData](#AesdRecords.RecordData).

*No more than on of `bookmark_id` or `expression` may be present in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The identifier for the model. |
| max_records | [uint64](#uint64) | optional | [semantically optional] If specified, this is the maximum number of records to return. Otherwise all records are returned, although they may be returned as multiple responses, each with a chunk of records. |
| var_ids | [int32](#int32) | repeated | [semantically optional] Which variables to include in the response. If this is not specified, all variables will be included. |
| bookmark_id | [string](#string) | optional | [semantically optional] Only respond with records in a specified bookmark. |
| expression | [FilterExpression](#AesdRecords.FilterExpression) | optional | [semantically optional] Only respond with records matching a specified criterion. |



### RequestSaveBookmark {#AesdRecords.RequestSaveBookmark}

A request to create or update a bookmark.

The response to this request is [BookmarkMetaList](#AesdRecords.BookmarkMetaList).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to save the bookmark. |
| new_bookmark | [BookmarkMeta](#AesdRecords.BookmarkMeta) | optional | [semantically optional] If empty, create a new bookmark. (In which case, leave the `bookmark_id` empty, so that the server will create a unique identifier for the new bookmark.) Otherwise, update an existing bookmark. |



### RequestWork {#AesdRecords.RequestWork}

Request that the server compute new records based on input values.

The response to this request is [RecordData](#AesdRecords.RecordData).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The identifier for the model. |
| inputs | [VarValue](#AesdRecords.VarValue) | repeated | [semantically optional] Which input variables to set to which values. |



### Response {#AesdRecords.Response}

A response to a request.

Note that a server may send multiple responses to a single request, expressed as a linked list of chunks. It is strongly recommended that servers chunk by `record_id` so that each record is kept intact. A chunk may be empty.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional | [semantically required] The version number for the API. *This must be the number **four**.* |
| id | [OptionalUInt32](#AesdRecords.OptionalUInt32) | optional | [semantically optional] A response without an identifier is a notification. Otherwise, the response identifier matches the response identifier for the original request. |
| chunk_id | [int32](#int32) | optional | [semantically optional, but recommended] The identifier for this chunk. It is recommended that chunks are number sequentially starting from one. |
| next_chunk_id | [int32](#int32) | optional | [semantically optional] The identifier of the next chunk, or zero if this is the last chunk. |
| error | [string](#string) | optional | An error message. |
| models | [ModelMetaList](#AesdRecords.ModelMetaList) | optional | A list of model metadata. |
| data | [RecordData](#AesdRecords.RecordData) | optional | A list of record data. |
| bookmarks | [BookmarkMetaList](#AesdRecords.BookmarkMetaList) | optional | A list of bookmark metadata. |



### StringList {#AesdRecords.StringList}

A list of character strings.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [string](#string) | repeated | [semantically required] The character strings. |



### Value {#AesdRecords.Value}

Value that may be a real number, an integer, or a character string

*Exactly one of `real_value`, `integer_value`, or `string_value` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| real_value | [double](#double) | optional | The real number. |
| integer_value | [int64](#int64) | optional | The integer. |
| string_value | [string](#string) | optional | The character string. |



### VarInterval {#AesdRecords.VarInterval}

A range of values of a [variable](#AesdRecords.VarMeta).

Both fields in this message are optional:

*   If neither field is present, then the interval designates all values in the domain.
*   If only `first_value`is present, then the interval designates all values starting from that value.
*   If only `last_value` is present, then the bookmark interval designates all values ending at that value.
*   If both fields are present, then the interval designates all values between the two values, inclusive.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| first_value | [Value](#AesdRecords.Value) | optional | [semantically optional] The first value in the interval. |
| last_value | [Value](#AesdRecords.Value) | optional | [semantically optional] The last value in the interval. |



### VarMeta {#AesdRecords.VarMeta}

Metadata for a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] A integer identifying the variable. |
| var_name | [string](#string) | optional | [semantically required] The name of the variable. |
| units | [string](#string) | optional | [semantically optional] The name of the unit of measure for values of the variable. |
| si | [sint32](#sint32) | repeated | [semantically optional] The unit of measure expressed as a list of the exponents for the eigth fundamental SI quantities [meter, kilogram, second, ampere, kelvin, mole, calenda, radian]. For example, the unit of acceleration $m/s^2$ would be express as `[1, 0, -2, 0, 0, 0, 0, 0]` because meters has an exponent of positive one and seconds has an exponent of negative two. |
| scale | [double](#double) | optional | [semantically optional] An overall scale relative to the fundamental SI scale of the unit of measure. For instance, kilometers would have a scale 1000 because the fundamental unit of distance is meters. |
| type | [VariableType](#AesdRecords.VariableType) | optional | [semantically optional] The data type for values of the variable. The default type is real number. |



### VarSet {#AesdRecords.VarSet}

A set of values for a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| elements | [Value](#AesdRecords.Value) | repeated | [semantically optional] The list of values in the set. |



### VarValue {#AesdRecords.VarValue}

The value of a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] The identifier for the variable. |
| value | [Value](#AesdRecords.Value) | optional | [semantically required] The value of the variable. |



### VariableType
The data type for a value.

| Name | Number | Description |
| ---- | ------ | ----------- |
| REAL | 0 | A real number. |
| INTEGER | 1 | An integer. |
| STRING | 2 | A character string. |





## Scalar Value Types

| .proto Type | Notes | C++ Type | Java Type | Python Type |
| ----------- | ----- | -------- | --------- | ----------- |
| double |  | double | double | float |
| float |  | float | float | float |
| int32 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint32 instead. | int32 | int | int |
| int64 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint64 instead. | int64 | long | int/long |
| uint32 | Uses variable-length encoding. | uint32 | int | int/long |
| uint64 | Uses variable-length encoding. | uint64 | long | int/long |
| sint32 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int32s. | int32 | int | int |
| sint64 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int64s. | int64 | long | int/long |
| fixed32 | Always four bytes. More efficient than uint32 if values are often greater than 2^28. | uint32 | int | int |
| fixed64 | Always eight bytes. More efficient than uint64 if values are often greater than 2^56. | uint64 | long | int/long |
| sfixed32 | Always four bytes. | int32 | int | int |
| sfixed64 | Always eight bytes. | int64 | long | int/long |
| bool |  | bool | boolean | boolean |
| string | A string must always contain UTF-8 encoded or 7-bit ASCII text. | string | String | str/unicode |
| bytes | May contain any arbitrary sequence of bytes. | string | ByteString | str |
