# Appendix
## Protocol Buffers for Records API Version 4

	syntax = "proto3";
	package AesdRecords;
	
	option optimize_for = LITE_RUNTIME;
	
	message OptionalInt32 {
	    int32 value = 1; /// [semantically required]
	}
	
	message OptionalUInt32 {
	    uint32 value = 1; /// [semantically required]
	}
	
	message OptionalString {
	    string value = 1; /// [semantically required]
	}
	
	message Value {
	    oneof    value              /// [semantically required]
	    {
	      double real_value    = 1; 
	      int64  integer_value = 2; 
	      string string_value  = 3; 
	    }
	}
	
	message DoubleList {
	    repeated double values = 1; /// [semantically required]
	}
	
	message IntegerList {
	    repeated sint64 values = 1; /// [semantically required]
	}
	
	message StringList {
	    repeated string values = 1; /// [semantically required]
	}
	
	message BookmarkIntervalContent {
	    int64 first_record = 1; /// [semantically optional]
	    int64 last_record  = 2; /// [semantically optional]
	}
	
	message BookmarkSetContent {
	    repeated int64 record_ids = 1; /// [semantically optional]
	}
	
	message BookmarkMeta {
	    string                      bookmark_id   = 1; /// [semantically optional]
	    string                      bookmark_name = 2; /// [semantically required]
	    oneof                       content            /// [semantically required]
	    {
	        BookmarkIntervalContent interval      = 3; 
	        BookmarkSetContent      set           = 4; 
	        FilterExpression        filter        = 5; 
	    }
	}
	
	message BookmarkMetaList {
	    repeated BookmarkMeta bookmark_metas = 1; /// [semantically optional]
	}
	
	message RequestBookmarkMeta {
	    string         model_id    = 1; /// [semantically required]
	    OptionalString bookmark_id = 2; /// [semantically optional]
	}
	
	message RequestSaveBookmark {
	    string       model_id     = 1; /// [semantically required]
	    BookmarkMeta new_bookmark = 2; /// [semantically optional]
	}
	
	message FilterExpression {
	  oneof                expression               /// [semantically required]
	  {
	    FilterNot          filter_not          = 1; 
	    FilterUnion        filter_union        = 2; 
	    FilterIntersection filter_intersection = 3; 
	    DomainMeta         filter_domain       = 4; 
	  }
	}
	
	message FilterNot {
	  FilterExpression filter_expression = 1; /// [semantically required]
	}
	
	message FilterUnion {
	  repeated FilterExpression filter_expressions = 1; /// [semantically required]
	}
	
	message FilterIntersection {
	  repeated FilterExpression filter_expressions = 1; /// [semantically required]
	}
	
	enum VariableType
	{
	    REAL            = 0; 
	    INTEGER         = 1; 
	    STRING          = 2; 
	}
	
	message VarMeta {
	    int32           var_id   = 1; /// [semantically required]
	    string          var_name = 2; /// [semantically required]
	    string          units    = 3; /// [semantically optional]
	    repeated sint32 si       = 4; /// [semantically optional]
	    double          scale    = 5; /// [semantically optional]
	    VariableType    type     = 6; /// [semantically optional]
	}
	
	message ModelMeta {
	    string              model_id   = 1; /// [semantically required]
	    string              model_name = 2; /// [semantically required]
	    string              model_uri  = 3; /// [semantically required]
	    repeated VarMeta    variables  = 4; /// [semantically required]
	    repeated DomainMeta inputs     = 5; /// [semantically optional]
	}
	
	message ModelMetaList {
	    repeated ModelMeta models = 1; /// [semantically optional]
	}
	
	message RequestModelsMeta {
	    OptionalString model_id = 1; /// [semantically optional]
	}
	
	message VarInterval {
	    Value first_value = 1; /// [semantically optional]
	    Value last_value  = 2; /// [semantically optional]
	}
	
	message VarSet {
	    repeated Value elements = 1; /// [semantically optional]
	}
	
	message DomainMeta {
	    int32           var_id   = 1; /// [semantically required]
	    oneof           domain        /// [semantically required]
	    {
	        VarInterval interval = 2; 
	        VarSet      set      = 3; 
	    }
	}
	
	message RequestWork {
	    string model_id          = 1; /// [semantically required]
	    repeated VarValue inputs = 2; /// [semantically optional]
	}
	
	message VarValue {
	    int32 var_id = 1; /// [semantically required]
	    Value value  = 2; /// [semantically required]
	}
	
	message Record {
	    int64    record_id          = 1; /// [semantically required]
	    repeated VarValue variables = 2; /// [semantically optional]
	}
	
	message RecordList {
	    repeated Record records = 1; /// [semantically optional]
	}
	
	message RecordTable {
	    repeated int32  var_ids    = 1; /// [semantically required]
	    repeated int64  rec_ids    = 2; /// [semantically required]
	    oneof           list            /// [semantically required]
	    {
	        DoubleList  reals      = 3; 
	        IntegerList integers   = 4; 
	        StringList  strings    = 5; 
	    }
	}
	
	message RecordData {
	    oneof           style      /// [semantically required]
	    {
	        RecordList  list  = 1; 
	        RecordTable table = 2; 
	    }
	}
	
	message RequestRecordsData {
	    string               model_id    = 1; /// [semantically required]
	    uint64               max_records = 2; /// [semantically optional]
	    repeated int32       var_ids     = 3; /// [semantically optional]
	    oneof                filter           /// [semantically optional]
	    {
	        string           bookmark_id = 4; /// [semantically optional]
	        FilterExpression expression  = 5; /// [semantically optional]
	    }
	}
	
	message Response {
	    uint32               version       = 1; /// [semantically required]
	    OptionalUInt32       id            = 2; /// [semantically optional]
	    int32                chunk_id      = 3; /// [semantically optional, but recommended]
	    int32                next_chunk_id = 4; /// [semantically optional]
	    oneof                type               /// [semantically optional]
	    {
	        string           error         = 5; 
	        ModelMetaList    models        = 6; 
	        RecordData       data          = 7; 
	        BookmarkMetaList bookmarks     = 8; 
	    }
	}
	
	message RequestCancel {
	    OptionalUInt32 id = 1; /// [semantically required]
	}
	
	message Request {
	    uint32                  version         = 1; /// [semantically required]
	    OptionalUInt32          id              = 2; /// [semantically optional, but recommended]
	    bool                    subscribe       = 3; /// [semantically optional]
	    oneof                   type                 /// [semantically required]
	    {
	        RequestModelsMeta   models_metadata = 4; 
	        RequestRecordsData  records_data    = 5; 
	        RequestBookmarkMeta bookmark_meta   = 6; 
	        RequestSaveBookmark save_bookmark   = 7; 
	        RequestCancel       cancel          = 8; 
	        RequestWork         work            = 9; 
	    }
	}
