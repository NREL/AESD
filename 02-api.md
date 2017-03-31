# ESDA Records API, Version 4

## General conventions

*  `var_id` is [int32](#int32)
*  `model_id` is [string](#string)
*  `record_id` is [int64](#int64)

Since all fields are optional in ProtoBuf 3, one cannot determine whether an optional value has been set or not if it is just a value, as opposed to a message.  That is not true for fields that are messages, where the absence of the field truly indicates that the value is absent, not just a default or unset value.  The message `OptionalString`, for example, is used in this API to indicate whether a character string value is truly present.  Thus `[RequestModelsMeta](#EsdaRecords.RequestModelsMeta)` has a `model_id` field that indicates whether the request is for all models, when the field has not been set, or for a specific one, when the field has been set.





### BookmarkIntervalContent {#EsdaRecords.BookmarkIntervalContent}

A range of [record identifiers](#EsdaRecords.Record) can specify the content of a [bookmark](#EsdaRecords.BookmarkMeta).  Bookmark interval content provides a convenient means to boomark a contiguous selection of records in a [model](#EsdaRecords.ModelMeta).

Both fields in this message are optional:

*   If neither field is present, then the bookmark interval designates all records in the model.
*   If only `first_record`is present, then the bookmark interval designates all records starting from that record identifier.
*   If only `last_record` is present, then the bookmark interval designates all records ending at that record identifier.  For a dynamic model, such a bookmark interval includes all "future" records.
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

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_id | [string](#string) | optional | [semantically optional] When creating a new bookmark, this field must be empty: the server will create a unique identifier for the bookmark.  This identifier uniquely identifies the bookmark *on the particular server*. |
| bookmark_name | [string](#string) | optional | [semantically required] A name for the bookmark, useful for displaying the bookmark to users. This need not be unique, although it is recommended to be so. |
| interval | [BookmarkIntervalContent](#EsdaRecords.BookmarkIntervalContent) | optional | The range of records in the bookmark.  *Exactly one of `interval`, `set`, or `filter` must be specified in this message.* |
| set | [BookmarkSetContent](#EsdaRecords.BookmarkSetContent) | optional | The list of records in the bookmark.  *Exactly one of `interval`, `set`, or `filter` must be specified in this message.* |
| filter | [FilterExpression](#EsdaRecords.FilterExpression) | optional | Logical conditions for defining which records are in the bookmark.  *Exactly one of `interval`, `set`, or `filter` must be specified in this message.* |



### BookmarkMetaList {#EsdaRecords.BookmarkMetaList}

Bookmarks may be grouped into lists (sets).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bookmark_metas | [BookmarkMeta](#EsdaRecords.BookmarkMeta) | repeated | [semantically optional] The bookmarks in the list. |



### BookmarkSetContent {#EsdaRecords.BookmarkSetContent}

A list (set) of [record identifiers](#Record) can specify the contents of a [bookmark](#EsdaRecords.BookmarkMeta).  Bookmark set content provides a convenient means to bookmark a specific selection of non-continuous records in a [model](#EsdaRecords.ModelMeta).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_ids | [int64](#int64) | repeated | [semantically optional] The list of record identifiers in the set. |



### DomainMeta {#EsdaRecords.DomainMeta}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [Semantically required. |
| interval | [VarInterval](#EsdaRecords.VarInterval) | optional |  |
| set | [VarSet](#EsdaRecords.VarSet) | optional |  |



### DoubleList {#EsdaRecords.DoubleList}

A list of real numbers.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [double](#double) | repeated | [semantically required] The real numbers. |



### FilterExpression {#EsdaRecords.FilterExpression}

A filtering expression is a composition of logical conditions on a [record](#EsdaRecords.Record).  It can be used to filter records.
There are four alternatives to specifying a filter expression:

1.  The [logical negation](#EsdaRecords.FilterNot) of another filtering expression.
2.  The [set union](#EsdaRecords.FilterUnion) of multiple filtering expressions.
3.  The [set intersection](#EsdaRecords.FilterIntersection) of multiple filtering expressions.
4.  [Particular values](#EsdaRecords.DomainMeta) of variables in a record.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_not | [FilterNot](#EsdaRecords.FilterNot) | optional | Logical negation of an expression.  *Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.* |
| filter_union | [FilterUnion](#EsdaRecords.FilterUnion) | optional | Set union of expressions.  *Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.* |
| filter_intersection | [FilterIntersection](#EsdaRecords.FilterIntersection) | optional | Set intersection of expressions.  *Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.* |
| filter_domain | [DomainMeta](#EsdaRecords.DomainMeta) | optional | Particular values of variables.  *Exactly one of `filter_not`, `filter_union`, `filter_intersection`, or `filter_domain` must be specified in this message.* |



### FilterIntersection {#EsdaRecords.FilterIntersection}

Set intersection of filtering expressions.  A record satisfies this expression if it satisfies all of `filter_expressions`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expressions | [FilterExpression](#EsdaRecords.FilterExpression) | repeated | [semantically required] The expressions to be intersected. |



### FilterNot {#EsdaRecords.FilterNot}

Logically negate a filtering expression.  A record satifies this expression if it does not satisfy `filter_expression`.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| filter_expression | [FilterExpression](#EsdaRecords.FilterExpression) | optional | [semantically required] The expression to be negated. |



### FilterUnion {#EsdaRecords.FilterUnion}

Set union of filtering expressions.  A record satisfies this expression if if satisfies any of `filter_expressions`.

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
| model_id | [string](#string) | optional | [Semantically required] The unique identifier for the model *on the particular server*. |
| model_name | [string](#string) | optional | [Semantically required] A name for the model, useful for display the model to users.  This need not be unique, although it is recommended to be so. |
| model_uri | [string](#string) | optional | [Semantically required] The unique URI for the model.  Additional metadata may be obtained by dereferencing that URI. |
| variables | [VarMeta](#EsdaRecords.VarMeta) | repeated | [Semantically required] Metadata for the variables. |
| inputs | [DomainMeta](#EsdaRecords.DomainMeta) | repeated | [Semantically optional] Metadata for input values to the model, if any. |



### ModelMetaList {#EsdaRecords.ModelMetaList}

A list of metadata for models.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| models | [ModelMeta](#EsdaRecords.ModelMeta) | repeated | [Semantically optional] The metadata for the models. |



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



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record_id | [int64](#int64) | optional | [Semantically required. |
| variables | [VarValue](#EsdaRecords.VarValue) | repeated | [Semantically optional. |



### RecordData {#EsdaRecords.RecordData}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| list | [RecordList](#EsdaRecords.RecordList) | optional |  |
| table | [RecordTable](#EsdaRecords.RecordTable) | optional |  |



### RecordList {#EsdaRecords.RecordList}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| records | [Record](#EsdaRecords.Record) | repeated | [Semantically optional. |



### RecordTable {#EsdaRecords.RecordTable}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_ids | [int32](#int32) | repeated | [Semantically required. |
| rec_ids | [int64](#int64) | repeated | [Semantically required. |
| reals | [DoubleList](#EsdaRecords.DoubleList) | optional |  |
| integers | [IntegerList](#EsdaRecords.IntegerList) | optional |  |
| strings | [StringList](#EsdaRecords.StringList) | optional |  |



### Request {#EsdaRecords.Request}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional | [Semantically required. |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [Semantically optional, but encouraged. |
| subscribe | [bool](#bool) | optional |  |
| models_metadata | [RequestModelsMeta](#EsdaRecords.RequestModelsMeta) | optional |  |
| records_data | [RequestRecordsData](#EsdaRecords.RequestRecordsData) | optional |  |
| bookmark_meta | [RequestBookmarkMeta](#EsdaRecords.RequestBookmarkMeta) | optional |  |
| save_bookmark | [RequestSaveBookmark](#EsdaRecords.RequestSaveBookmark) | optional |  |
| cancel | [RequestCancel](#EsdaRecords.RequestCancel) | optional |  |
| work | [RequestWork](#EsdaRecords.RequestWork) | optional |  |



### RequestBookmarkMeta {#EsdaRecords.RequestBookmarkMeta}

A request for one or more [bookmarks](#EsdaRecords.BookmarkMetaList) for a [model](#EsdaRecords.ModelMeta).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to list bookmarks. |
| bookmark_id | [OptionalString](#EsdaRecords.OptionalString) | optional | [semantically optional] If empty, list all bookmarks for the model.  Otherwise, list just the bookmark metadata for this specific bookmark identifier. |



### RequestCancel {#EsdaRecords.RequestCancel}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [Semantically required, specifying which previous |



### RequestModelsMeta {#EsdaRecords.RequestModelsMeta}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [OptionalString](#EsdaRecords.OptionalString) | optional | [Semantically optional: ask for specific one, |



### RequestRecordsData {#EsdaRecords.RequestRecordsData}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [Semantically required. |
| max_records | [uint64](#uint64) | optional | [Semantically optional: specify a limit, so you |
| var_ids | [int32](#int32) | repeated | [Semantically optional: specify the variables |
| bookmark_id | [string](#string) | optional | [Semantically optional: if filled |
| expression | [FilterExpression](#EsdaRecords.FilterExpression) | optional | [Semantically optional: if provided, |



### RequestSaveBookmark {#EsdaRecords.RequestSaveBookmark}

A request to create or update a [bookmark](#EsdaRecords.BookmarkMeta).

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional | [semantically required] Which model for which to save the bookmark. |
| new_bookmark | [BookmarkMeta](#EsdaRecords.BookmarkMeta) | optional | [semantically optional] If empty, create a new bookmark.  (In which case, leave the `bookmark_id` empty, so that the server will create a unique identifier for the new bookmark.) Otherwise, update an existing bookmark. |



### RequestWork {#EsdaRecords.RequestWork}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| model_id | [string](#string) | optional |  |
| inputs | [VarValue](#EsdaRecords.VarValue) | repeated | [Semantically optional, specifying which input variables to set. |



### Response {#EsdaRecords.Response}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| version | [uint32](#uint32) | optional |  |
| id | [OptionalUInt32](#EsdaRecords.OptionalUInt32) | optional | [Semantically optional, but encourgaged. |
| chunk_id | [int32](#int32) | optional | [Semantically optional: the id of this chunk 1 - N |
| next_chunk_id | [int32](#int32) | optional | [Semantically optional: the id to expect next, or 0 if its the last |
| error | [string](#string) | optional |  |
| models | [ModelMetaList](#EsdaRecords.ModelMetaList) | optional |  |
| data | [RecordData](#EsdaRecords.RecordData) | optional |  |
| bookmarks | [BookmarkMetaList](#EsdaRecords.BookmarkMetaList) | optional |  |



### StringList {#EsdaRecords.StringList}

A list of character strings.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| values | [string](#string) | repeated | [semantically required] The character strings. |



### Value {#EsdaRecords.Value}

Value that may be a real number, an integer, or a character string.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| real_value | [double](#double) | optional | The real number.  *Exactly one of `real_value`, `integer_value`, or `string_value` must be specified in this message.* |
| integer_value | [int64](#int64) | optional | The integer.  *Exactly one of `real_value`, `integer_value`, or `string_value` must be specified in this message.* |
| string_value | [string](#string) | optional | The character string.  *Exactly one of `real_value`, `integer_value`, or `string_value` must be specified in this message.* |



### VarInterval {#EsdaRecords.VarInterval}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| first_value | [Value](#EsdaRecords.Value) | optional | [Semantically optional. |
| last_value | [Value](#EsdaRecords.Value) | optional | [Semantically optional. |



### VarMeta {#EsdaRecords.VarMeta}

Metadata for a variable.

| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [Semantically required] A integer identifying the variable. |
| var_name | [string](#string) | optional | [Semantically required] The name of the variable. |
| units | [string](#string) | optional | [Semantically optional] The name of the unit of measure for values of the variable. |
| si | [sint32](#sint32) | repeated | [Semantically optional] The unit of measure expressed as a list of the exponents for the eigth fundamental SI quantities [meter, kilogram, second, ampere, kelvin, mole, calenda, radian].  For example, the unit of acceleration $m/s^2$ would be express as [1, 0, -2, 0, 0, 0, 0, 0] because meters has an exponent of positive one and seconds has an exponent of negative two. |
| scale | [double](#double) | optional | [Semantically optional] An overall scale relative to the fundamental SI scale of the unit of measure.  For instance, kilometers would have a scale 1000 because the fundamental unit of distance is meters. |
| type | [VarMeta.VariableType](#EsdaRecords.VarMeta.VariableType) | optional | [Semanatically optional] The data type for values of the variable. The default type is real number. |



### VarSet {#EsdaRecords.VarSet}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| elements | [Value](#EsdaRecords.Value) | repeated | [Semantically optional. |



### VarValue {#EsdaRecords.VarValue}



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_id | [int32](#int32) | optional | [Semantically required. |
| value | [Value](#EsdaRecords.Value) | optional | [Semantically required. |



### VarMeta.VariableType
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
