
# ESDA Records API, Version 4

## Contents

*   Requests and responses
    *   [Request](#EsdaRecords.Request)
    *   [RequestModelsMeta](#EsdaRecords.RequestModelsMeta)
    *   [RequestRecordsData](#EsdaRecords.RequestRecordsData)
    *   [RequestWork](#EsdaRecords.RequestWork)
    *   [RequestBoomarkMeta](#EsdaRecords.RequestBoomarkMeta)
    *   [RequestSaveBookmark](#EsdaRecords.RequestSaveBookmark)
    *   [RequestCancel](#EsdaRecords.RequestCancel)
    *   [Response](#EsdaRecords.Response)
*   Model and variable metadata
    *   [ModelMeta](#EsdaRecords.ModelMeta)
    *   [ModelMetaList](#EsdaRecords.ModelMetaList)
    *   [DomainMeta](#EsdaRecords.DomainMeta)
    *   [VarMeta](#EsdaRecords.VarMeta)
    *   [VariableType](#EsdaRecords.VariableType)
    *   [VarSet](#EsdaRecords.VarSet)
    *   [VarInterval](#EsdaRecords.VarInterval)
*   Data records and values
    *   [Record](#EsdaRecords.Record)
    *   [VarValue](#EsdaRecords.VarValue)
    *   [Value](#EsdaRecords.Value)
    *   [RecordData](#EsdaRecords.RecordData)
    *   [RecordList](#EsdaRecords.RecordList)
    *   [RecordTable](#EsdaRecords.RecordTable)
*   Filtering
    *   [FilterExpression](#EsdaRecords.FilterExpression)
    *   [FilterNot](#EsdaRecords.FilterNot)
    *   [FilterIntersection](#EsdaRecords.FilterIntersection)
    *   [FilterUnion](#EsdaRecords.FilterUnion)
*   Bookmarks
    *   [BookmarkMeta](#EsdaRecords.BookmarkMeta)
    *   [BookmarkMetaList](#EsdaRecords.BookmarkMetaList)
    *   [BookmarkIntervalContent](#EsdaRecords.BookmarkIntervalContent)
    *   [BookmarkSetContent](#EsdaRecords.BookmarkSetContent)
*   Miscellaneous utility messages
    *   [DoubleList](#EsdaRecords.DoubleList)
    *   [IntegerList](#EsdaRecords.IntegerList)
    *   [StringList](#EsdaRecords.StringList)
    *   [OptionalInt32](#EsdaRecords.OptionalInt32)
    *   [OptionalUInt32](#EsdaRecords.OptionalUInt32)
    *   [OptionalString](#EsdaRecords.OptionalString)

## General conventions

All fields are technically optional in ProtoBuf 3, but some fields may be required in each message type in order for the message to be semantically valid.  In the following specifications for the messages, fields are annotated as *semantically required* or *semantically optional*.  Also, the specification notes when field in the [protobuf `oneof` construct](https://developers.google.com/protocol-buffers/docs/proto3#oneof) are required or mutually exclusive.

Furthermore, one cannot determine whether an optional value has been set or not if it is just a value, as opposed to a message. That is not true for fields that are messages, where the absence of the field truly indicates that the value is absent, not just a default or unset value. The message `OptionalString`, for example, is used in this API to indicate whether a character string value is truly present. Thus [`RequestModelsMeta`](#EsdaRecords.RequestModelsMeta) has a `model_id` field that indicates whether the request is for all models, when the field has not been set, or for a specific one, when the field has been set.

Throughout this specification, the following types are used for identifiers:
*  `var_id` is [int32](#int32)
*  `model_id` is [string](#string)
*  `record_id` is [int64](#int64)

This specification conforms to [Protocol Buffers version 3](https://developers.google.com/protocol-buffers/docs/proto3).


### BookmarkIntervalContent {#EsdaRecords.BookmarkIntervalContent}

A range of [record identifiers](#EsdaRecords.Record) can specify the content of a [bookmark](#EsdaRecords.BookmarkMeta). Bookmark interval content provides a convenient means to boomark a contiguous selection of records in a [model](#EsdaRecords.ModelMeta).

Both fields in this message are optional:

*   If neither field is present, then the bookmark interval designates all records in the model.
*   If only `first_record`is present, then the bookmark interval designates all records starting from that record identifier.
*   If only `last_record` is present, then the bookmark interval designates all records ending at that record identifier. For a dynamic model, such a bookmark interval includes all "future" records.
*   If both fields are present, then the bookmark interval designates all records between the two identifiers, inclusive.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| first_record | [int64](#int64) | optional | [semantically optional] The identifier for the first record in the interval. |
| last_record | [int64](#int64) | optional | [semantically optional] The identifier for the last record in the interval. |



### BookmarkMeta {#EsdaRecords.BookmarkMeta}

A bookmark is metadata defining a subset of records in a [model](#EsdaRecords.ModelMeta).

There are three alternatives to specifying a bookmark:

1.  [Interval content](#EsdaRecords.BookmarkIntervalContent) specifies a range of records in the bookmark.
2.  [Set content](#EsdaRecords.BookmarkSetContent) specifies a list of records in the bookmark.
3.  A [filter expression](#EsdaRecords.FilterExpression) defines a set of logical conditions for determining whether a record is in the bookmrk.

*Exactly one of `interval`, `set`, or `filter` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_id | [string](#string) | optional | [semantically optional] When creating a new bookmark, this field must be empty: the server will create a unique identifier for the bookmark. This identifier uniquely identifies the bookmark *on the particular server*. |
| bookmark_name | [string](#string) | optional | [semantically required] A name for the bookmark, useful for displaying the bookmark to users. This need not be unique, although it is recommended to be so. |
| interval | [BookmarkIntervalContent](#EsdaRecords.BookmarkIntervalContent) | optional | The range of records in the bookmark. |
| set | [BookmarkSetContent](#EsdaRecords.BookmarkSetContent) | optional | The list of records in the bookmark. |
| filter | [FilterExpression](#EsdaRecords.FilterExpression) | optional | Logical conditions for defining which records are in the bookmark. |



### BookmarkMetaList {#EsdaRecords.BookmarkMetaList}

Bookmarks may be grouped into lists (sets).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_metas | [BookmarkMeta](#EsdaRecords.BookmarkMeta) | repeated | [semantically optional] The bookmarks in the list. |



### BookmarkSetContent {#EsdaRecords.BookmarkSetContent}

A list (set) of [record identifiers](#EsdaRecords.Record) can specify the contents of a [bookmark](#EsdaRecords.BookmarkMeta). Bookmark set content provides a convenient means to bookmark a specific selection of non-continuous records in a [model](#EsdaRecords.ModelMeta).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_ids | [int64](#int64) | repeated | [semantically optional] The list of record identifiers in the set. |



### DomainMeta {#EsdaRecords.DomainMeta}

The domain (set of valid values) for a variable.

There are two alternatives to specifying a domain:

1.  [An interval](#EsdaRecords.VarInterval) specifies a range of values in the domain.
2.  [A set](#EsdaRecords.VarSet) specifies a list of values in the domain.

*Exactly one of `interval` or `set` must be specified in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] |
| interval | [VarInterval](#EsdaRecords.VarInterval) | optional | The interval of values in the domain. |
| set | [VarSet](#EsdaRecords.VarSet) | optional | The list of values in the domain. |



### DoubleList {#EsdaRecords.DoubleList}

A list of real numbers.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [double](#double) | repeated | [semantically required] The real numbers. |



### FilterExpression {#EsdaRecords.FilterExpression}

A filtering expression is a composition of logical conditions on a [record](#EsdaRecords.Record). It can be used to filter records.
There are four alternatives to specifying a filter expression:

1.  The [logical negation](#EsdaRecords.FilterNot) of another filtering expression.
2.  The [set union](#EsdaRecords.FilterUnion) of multiple filtering expressions.
3.  The [set intersection](#EsdaRecords.FilterIntersection) of multiple filtering expressions.
4.  [Particular values](#EsdaRecords.DomainMeta) of variables in a record.

*Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_not | [FilterNot](#EsdaRecords.FilterNot) | optional | Logical negation of an expression. |
| filter_union | [FilterUnion](#EsdaRecords.FilterUnion) | optional | Set union of expressions. |
| filter_intersection | [FilterIntersection](#EsdaRecords.FilterIntersection) | optional | Set intersection of expressions. |
| filter_domain | [DomainMeta](#EsdaRecords.DomainMeta) | optional | Particular values of variables. |



### FilterIntersection {#EsdaRecords.FilterIntersection}

Set intersection of filtering expressions. A record satisfies this expression if it satisfies all of `filter_expressions`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expressions | [FilterExpression](#EsdaRecords.FilterExpression) | repeated | [semantically required] The expressions to be intersected. |



### FilterNot {#EsdaRecords.FilterNot}

Logically negate a filtering expression. A record satifies this expression if it does not satisfy `filter_expression`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expression | [FilterExpression](#EsdaRecords.FilterExpression) | optional | [semantically required] The expression to be negated. |



### FilterUnion {#EsdaRecords.FilterUnion}

Set union of filtering expressions. A record satisfies this expression if if satisfies any of `filter_expressions`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expressions | [FilterExpression](#EsdaRecords.FilterExpression) | repeated | [semantically required] The expressions to be unioned. |



### IntegerList {#EsdaRecords.IntegerList}

A list of integers.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [sint64](#sint64) | repeated | [semantically required] The integers. |



### ModelMeta {#EsdaRecords.ModelMeta}

Metadata for a model.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The unique identifier for the model *on the particular server*. |
| model_name | [string](#string) | optional | [semantically required] A name for the model, useful for display the model to users. This need not be unique, although it is recommended to be so. |
| model_uri | [string](#string) | optional | [semantically required] The unique URI for the model. Additional metadata may be obtained by dereferencing that URI. |
| variables | [VarMeta](#EsdaRecords.VarMeta) | repeated | [semantically required] Metadata for the variables. |
| inputs | [DomainMeta](#EsdaRecords.DomainMeta) | repeated | [semantically optional] Metadata for input values to the model, if any. |



### ModelMetaList {#EsdaRecords.ModelMetaList}

A list of metadata for models.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| models | [ModelMeta](#EsdaRecords.ModelMeta) | repeated | [semantically optional] The metadata for the models. |



### OptionalInt32 {#EsdaRecords.OptionalInt32}

Wrapper for an optional signed integer.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [int32](#int32) | optional | [semantically required] The signed integer value. |



### OptionalString {#EsdaRecords.OptionalString}

Wrapper for an optional string.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [string](#string) | optional | [semantically required] The character string value. |



### OptionalUInt32 {#EsdaRecords.OptionalUInt32}

Wrapper for an optional unsigned integer.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| value | [uint32](#uint32) | optional | [semantically required] The unsigned integer value. |



### Record {#EsdaRecords.Record}

A record is a list of variables and their associated values.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_id | [int64](#int64) | optional | [semantically required] A unique identifier for the record. |
| variables | [VarValue](#EsdaRecords.VarValue) | repeated | [semantically optional] The values for variables in the record. |



### RecordData {#EsdaRecords.RecordData}

A collection or records.

There are two alternatives to specifying record data:

1.  [A list](#EsdaRecords.RecordList) specifies a heterogeneously typee list.
2.  [A table](#EsdaRecords.RecordTable) specifies a homogeneously typed table.

 *Exactly one of `list` or `table` must be present in the messsage.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| list | [RecordList](#EsdaRecords.RecordList) | optional | A heterogeneously typed list of records. |
| table | [RecordTable](#EsdaRecords.RecordTable) | optional | A homogeneously typed table of records. |



### RecordList {#EsdaRecords.RecordList}

A list of records. The list is heterogeneous in the sense that each variable may have a different type.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| records | [Record](#EsdaRecords.Record) | repeated | [semantically optional] The list of records. |



### RecordTable {#EsdaRecords.RecordTable}

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
| reals | [DoubleList](#EsdaRecords.DoubleList) | optional | The real numbers comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |
| integers | [IntegerList](#EsdaRecords.IntegerList) | optional | The integers comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |
| strings | [StringList](#EsdaRecords.StringList) | optional | The character strings comprising the values of the variables, in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order). |



### Request {#EsdaRecords.Request}

A request. There are six types of requests:

| Request                        | Response                                          |
|--------------------------------|---------------------------------------------------|
| Metadata for model(s)          | [ModelMetaList](#EsdaRecords.ModelMetaList)       |
| Data records                   | [RecordData](#EsdaRecords.RecordData)             |
| Metadata for bookmark(s)       | [BookmarkMetaList](#EsdaRecords.BookmarkMetaList) |
| Saving a bookmark              | [BookmarkMetaList](#EsdaRecords.BookmarkMetaList) |
| Canceling a previous request   | n/a                                               |
| New work, such as a simulation | [RecordData](#EsdaRecords.RecordData)             |

*Exactly one of `models_metadata`, `records_data`, `bookmark_meta`, `save_bookmark`, `cancel`, or `work` must be specified in the message.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional | [semantically required] The version number for the API. *This must be the number **four**.* |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [semantically optional, but recommended] An identifier which will be used to tag responses, so that responses can be correlated with requests. |
| subscribe | [bool](#bool) | optional | [semantically optional] Whether to continue receiving responses indefinitely, as new records become available. This is useful, for example, when a sensor is reporting measurements periodically or when simulations are reporting a series or results. Use [RequestCancel](#EsdaRecords.RequestCancel) to end the subscription. |
| models_metadata | [RequestModelsMeta](#EsdaRecords.RequestModelsMeta) | optional | Request metadata for model(s). |
| records_data | [RequestRecordsData](#EsdaRecords.RequestRecordsData) | optional | Request data records. |
| bookmark_meta | [RequestBookmarkMeta](#EsdaRecords.RequestBookmarkMeta) | optional | Request metadata for bookmark(s). |
| save_bookmark | [RequestSaveBookmark](#EsdaRecords.RequestSaveBookmark) | optional | Request save a new bookmark or update an existing one. |
| cancel | [RequestCancel](#EsdaRecords.RequestCancel) | optional | Request cancel a previous request). |
| work | [RequestWork](#EsdaRecords.RequestWork) | optional | Request request work (e.g., simulation results). |



### RequestBookmarkMeta {#EsdaRecords.RequestBookmarkMeta}

A request for one or more bookmarks for a [model](#EsdaRecords.ModelMeta).

The response to this request is [BookmarkMetaList](#EsdaRecords.MetaList)

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to list bookmarks. |
| bookmark_id | [OptionalString](#EsdaRecords.OptionalString) | optional | [semantically optional] If empty, list all bookmarks for the model. Otherwise, list just the bookmark metadata for this specific bookmark identifier. |



### RequestCancel {#EsdaRecords.RequestCancel}

Cancel a previous request.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [semantically required] Which request to cancel. |



### RequestModelsMeta {#EsdaRecords.RequestModelsMeta}

A request for metadata about model(s).

The response to this request is [ModelMetaList](#EsdaRecords.ModelMetaList).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [OptionalString](#EsdaRecords.OptionalString) | optional | [semantically optional] If absent, the request is for metadata for all models. Otherwise the request is for the specifically identified model. |



### RequestRecordsData {#EsdaRecords.RequestRecordsData}

Request record data for a model.

There are three alternatives to requesting record data.

1.  Request all records.
2.  Request records in [a bookmark](#EsdaRecords.BookmarkMeta).
3.  [Filter](#EsdaRecords.FilterExpression) records according to a criterion.

The response to this request is [RecordData](#EsdaRecords.RecordData).

*No more than on of `bookmark_id` or `expression` may be present in the message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The identifier for the model. |
| max_records | [uint64](#uint64) | optional | [semantically optional] If specified, this is the maximum number of records to return. Otherwise all records are returned, although they may be returned as multiple responses, each with a chunk of records. |
| var_ids | [int32](#int32) | repeated | [semantically optional] Which variables to include in the response. If this is not specified, all variables will be included. |
| bookmark_id | [string](#string) | optional | [semantically optional] Only respond with records in a specified bookmark. |
| expression | [FilterExpression](#EsdaRecords.FilterExpression) | optional | [semantically optional] Only respond with records matching a specified criterion. |



### RequestSaveBookmark {#EsdaRecords.RequestSaveBookmark}

A request to create or update a bookmark.

The response to this request is [BookmarkMetaList](#EsdaRecords.BookmarkMetaList).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to save the bookmark. |
| new_bookmark | [BookmarkMeta](#EsdaRecords.BookmarkMeta) | optional | [semantically optional] If empty, create a new bookmark. (In which case, leave the `bookmark_id` empty, so that the server will create a unique identifier for the new bookmark.) Otherwise, update an existing bookmark. |



### RequestWork {#EsdaRecords.RequestWork}

Request that the server compute new records based on input values.

The response to this request is [RecordData](#EsdaRecords.RecordData).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] The identifier for the model. |
| inputs | [VarValue](#EsdaRecords.VarValue) | repeated | [semantically optional, specifying which input variables to set. |



### Response {#EsdaRecords.Response}

A response to a request.

Note that a server may send multiple responses to a single request, expressed as a linked list of chunks. It is strongly recommended that servers chunk by `record_id` so that each record is kept intact. A chunk may be empty.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional | [semantically required] The version number for the API. *This must be the number **four**.* |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [semantically optional] A response without an identifier is a notification. Otherwise, the response identifier matches the response identifier for the original request. |
| chunk_id | [int32](#int32) | optional | [semantically optional, but recommended] The identifier for this chunk. It is recommended that chunks are number sequentially starting from one. |
| next_chunk_id | [int32](#int32) | optional | [semantically optional] The identifier of the next chunk, or zero if this is the last chunk. |
| error | [string](#string) | optional | An error message. |
| models | [ModelMetaList](#EsdaRecords.ModelMetaList) | optional | A list of model metadata. |
| data | [RecordData](#EsdaRecords.RecordData) | optional | A list of record data. |
| bookmarks | [BookmarkMetaList](#EsdaRecords.BookmarkMetaList) | optional | A list of bookmark metadata. |



### StringList {#EsdaRecords.StringList}

A list of character strings.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [string](#string) | repeated | [semantically required] The character strings. |



### Value {#EsdaRecords.Value}

Value that may be a real number, an integer, or a character string

*Exactly one of `real_value`, `integer_value`, or `string_value` must be specified in this message.*

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| real_value | [double](#double) | optional | The real number. |
| integer_value | [int64](#int64) | optional | The integer. |
| string_value | [string](#string) | optional | The character string. |



### VarInterval {#EsdaRecords.VarInterval}

A range of values of a [variable](#EsdaRecords.VarMeta).

Both fields in this message are optional:

*   If neither field is present, then the interval designates all values in the domain.
*   If only `first_value`is present, then the interval designates all values starting from that value.
*   If only `last_value` is present, then the bookmark interval designates all values ending at that value.
*   If both fields are present, then the interval designates all values between the two values, inclusive.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| first_value | [Value](#EsdaRecords.Value) | optional | [semantically optional] The first value in the interval. |
| last_value | [Value](#EsdaRecords.Value) | optional | [semantically optional] The last value in the interval. |



### VarMeta {#EsdaRecords.VarMeta}

Metadata for a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] A integer identifying the variable. |
| var_name | [string](#string) | optional | [semantically required] The name of the variable. |
| units | [string](#string) | optional | [semantically optional] The name of the unit of measure for values of the variable. |
| si | [sint32](#sint32) | repeated | [semantically optional] The unit of measure expressed as a list of the exponents for the eigth fundamental SI quantities [meter, kilogram, second, ampere, kelvin, mole, calenda, radian]. For example, the unit of acceleration $m/s^2$ would be express as `[1, 0, -2, 0, 0, 0, 0, 0]` because meters has an exponent of positive one and seconds has an exponent of negative two. |
| scale | [double](#double) | optional | [semantically optional] An overall scale relative to the fundamental SI scale of the unit of measure. For instance, kilometers would have a scale 1000 because the fundamental unit of distance is meters. |
| type | [VariableType](#EsdaRecords.VariableType) | optional | [semanatically optional] The data type for values of the variable. The default type is real number. |



### VarSet {#EsdaRecords.VarSet}

A set of values for a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| elements | [Value](#EsdaRecords.Value) | repeated | [semantically optional] The list of values in the set. |



### VarValue {#EsdaRecords.VarValue}

The value of a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [semantically required] The identifier for the variable. |
| value | [Value](#EsdaRecords.Value) | optional | [semantically required] The value of the variable. |



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
