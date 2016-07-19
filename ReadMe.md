# CESDS Records API in Haskell

## Overview

This [Haskell package](cesds-records.cabal) contains . . .

1.  a library for working with the CESDS Records API,
2.  [a skeletal server](src/CESDS/Server.hs) for the REST API
3.  [automated tests](src/TestJSON.hs) for the library, and
4.  [an example server](src/Main.hs) that implements the REST API and provides random, but consistent data and validates input.

This implementation comforms to http://github.nrel.gov:kgruchal/cesds/commit/8c3b1aaca522cf26ec466cf9e7177e20b7ebdcb4, but see the note in the [change log](ChangeLog.md).

## Example usage of CESDS API

### Get server status

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/'
	
	{
	  "status":{"state":"ok","message":"SWjkS9xkJ7WQ481"},
	  "server_id":"2LpkwQF386kJli07eBkbQMl3FCom",
	  "server_type":"record_server",
	  "models":["w","Q"],
	  "version":1
	}

### Attempt a request with malformed JSON

	$ curl -H POST --data-ascii '{"command": "restart}' 'http://1lv11lamb01.nrel.gov:8090/command'
	
	illegal JSON: Error in $: not enough input

### Attempt a request with a missing parameter

	$ curl -H POST --data-ascii '{"param" : ["a parameter"]}' 'http://1lv11lamb01.nrel.gov:8090/command'
	
	illegal JSON: Error in $: key "command" not present

### Experience server failure

	$ curl -H POST --data-ascii '{"command" : "restart", "param" : []}' 'http://1lv11lamb01.nrel.gov:8090/command'
	
	{"result":"random failure for testing"}

### Restart the server

	$ curl -H POST --data-ascii '{"command" : "restart"}' 'http://1lv11lamb01.nrel.gov:8090/command'
	
	{"result":"xTr8IE3Vl78O3J6Eqngh6n2FGUYxl"}

### Retrieve model information

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF'
	
	{
	  "record_id_var":"9hn",
	  "variables":[
	    {
	      "display":{"shortlabel":"ig","label":"s"},
	      "domain":{"set":{"options":["1Vcv","IJ2","bNE3","E6O"]}},
	      "is_input":true,
	      "var_id":"9hn"
	    },{
	      "display":{"color":"#010201","shortlabel":"J","label":"dDOW"},
	      "domain":{"set":{"options":["7tY","YPdZ"]}},
	      "is_input":false,
	      "var_id":"H9Bf"
	    },{
	      "display":{"color":"#020001","shortlabel":"xMz","label":"6NfV"},
	      "domain":{"set":{"options":["Tr","SBm"]}},
	      "is_input":true,
	      "var_id":"EwHT"
	    },{
	      "display":{"shortlabel":"V5x","label":"J"},
	      "domain":{"set":{"options":["x","Ql6h"]}},
	      "is_input":true,
	      "var_id":"2"
	    }
	  ],
	  "model_id":"JCF",
	  "time_key":"H9Bf",
	  "generation":-29,
	  "description":"ZwSEd57QUlNYOum",
	  "label":"YscJ8JyshhQaL3V33cHcCaNUg9",
	  "record_count":4
	}

### Set the strategy for running simulations

	$ curl -H POST --data-ascii '{"command" : "set_model_strategy", "param" : ["FIFO"]}' 'http://1lv11lamb01.nrel.gov:8090/models/SaQ/command'
	
	{"result":"strategy request ignored"}

### Retrieve records

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/records?from=-30'
	
	[
	  {
	    "id":"uno",
	    "variables":[
	      {"id":"9hn","value":"E6O"},
	      {"id":"2","value":"Ql6h"},
	      {"id":"H9Bf","value":"7tY"},
	      {"id":"EwHT","value":"SBm"}
	    ]
	  },
	  {
	    "id":"duo",
	    "variables":[
	      {"id":"9hn","value":"bNE3"},
	      {"id":"2","value":"Ql6h"},
	      {"id":"H9Bf","value":"7tY"},
	      {"id":"EwHT","value":"Tr"}
	    ]
	  },
	  {
	    "id":"tre",
	    "variables":[
	      {"id":"9hn","value":"1Vcv"},
	      {"id":"2","value":"Ql6h"},
	      {"id":"H9Bf","value":"YPdZ"},
	      {"id":"EwHT","value":"Tr"}
	    ]
	  }
	]
	

### Attempt to submit malformed work

	$ curl -H POST --data-ascii '{"explicit" : {"9hn" : 20}, "random" : ["E60"]}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	cannot set value of output variable
	
	$ curl -H POST --data-ascii '{"explicit" : {"EwHT" : 20}, "random" : []}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	value not in domain
	
	$ curl -H POST --data-ascii '{"explicit" : {"EwHT" : "invalid"}, "random" : []}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	value incompatible with domain	

### List work and watch for it to complete

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	[
	  {"status":"pending","work_id":"GYIs2"},
	  {"status":"running","work_id":"zGuZx4tzHalWLvmNOOsSTMqsc"},
	  {"status":"success","work_id":"IC5P65YZEWaDoCz","result_id":"V5efDKAU8f7"},
	  {"status":"success","work_id":"eUTB6l5JXkQYOHei9","result_id":"hA4fPQQEpLKbvwNW5"}
	]
	
	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	[
	  {"status":"running","work_id":"GYIs2"},
	  {"status":"failed","work_id":"zGuZx4tzHalWLvmNOOsSTMqsc","additional" : "random failure for testing"},
	  {"status":"success","work_id":"IC5P65YZEWaDoCz","result_id":"V5efDKAU8f7"},
	  {"status":"success","work_id":"eUTB6l5JXkQYOHei9","result_id":"hA4fPQQEpLKbvwNW5"}
	]
	
	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	[
	  {"status":"success","work_id":"GYIs2","result_id":"KCovQuF"},
	  {"status":"failed","work_id":"zGuZx4tzHalWLvmNOOsSTMqsc","additional" : "random failure for testing"},
	  {"status":"success","work_id":"IC5P65YZEWaDoCz","result_id":"V5efDKAU8f7"},
	  {"status":"success","work_id":"eUTB6l5JXkQYOHei9","result_id":"hA4fPQQEpLKbvwNW5"}
	]

### Submit work

	$ curl -H POST --data-ascii '{"explicit" : {"EwHT" : "Tr"}, "random" : []}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	{"work_id":"GYIs2","generation":-28}

### Retrieve results

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/records/KCovQuF'
	
	{
	  "id":"qua",
	  "variables":{
	    {"id":"9hn","value":"E6O"},
	    {"id":"2","value":"Ql6h"},
	    {"id":"H9Bf","value":"7tY"},
	    {"id":"EwHT","value":"SBm"}
	}
	
	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/records?from=-28'
	
	[
	  {"id":"qua",
	  "variables":{
	    {"id":"9hn","value":"E6O"},
	    {"id":"2","value":"Ql6h"},
	    {"id":"H9Bf","value":"7tY"},
	    {"id":"EwHT","value":"SBm"}
	  }
	]

### Attempt to submit work with duplicate primary keys

	$ curl -H POST --data-ascii '{"explicit" : {"9hn" : "E6O"}, "random" : []}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/work'
	
	primary key violation

### List bookmark metadata

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks'
	
	["myl7t8npVZbCPOCLCg"]

### List bookmark

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks/myl7t8npVZbCPOCLCg'
	
	{
	  "record_ids":["Oh2yztzSCgDY","hA4fPQQEpLKbvwNW5"],
	  "meta":{
	    "count":2,
	    "name":"jFfPeFMGsb4YH",
	    "bookmark_id":"myl7t8npVZbCPOCLCg",
	    "tags":{"ZQ":"Ry","QV":4.518413462458594}
	  }
	}

### Search bookmarks by tag

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks?ZQ="Ry"'
	
	["myl7t8npVZbCPOCLCg"]

### Add a bookmark

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "count" : 1}, "record_ids" : ["Oh2yztzSCgDY"]}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks'
	
	{
	  "record_ids":["Oh2yztzSCgDY"],
	  "meta":{
	    "count":1,
	    "name":"test bookmark",
	    "bookmark_id":"CLpAIHa"
	  }
	}

### Retrieve a bookmark

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks/CLpAIHa'
	
	{
	  "record_ids":["Oh2yztzSCgDY"],
	  "meta":{
	    "count":1,
	    "name":"test bookmark",
	    "bookmark_id":"CLpAIHa"
	  }
	}

### Attempt to add a bookmark incorrectly referencing records

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "count" : 1}, "record_ids" : ["not a record"]}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/bookmarks'
	
	invalid record identifiers

### List filter metadata

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/filters'
	
	[
	  "Cg7RdpzZYr0bs7",
	  "pOeN9UfCdekdFUwh6uBlYrCYxPCN6",
	  "88J33wDmU",
	  "0rZRFzzLlhE2o888Rftl7J10YwfAEs5",
	  "qKsRreE8h04v",
	  "Bwm",
	  "FUH2Agfzn3l09IFFkQ6m8Es",
	  "A2cmZsolppMhHqSsdBLFK48vOa",
	  "HqX6ReFe1xr3fc6XiwNP",
	  "rZ2vZnY",
	  "5CVCeVJ7jcyaCHIbXy5YoX",
	  "GYNOqFtD",
	  "phxLzqoK",
	  "URK8NicF2HUIJKZNNzAayjRLqhm",
	  "LhViaZEc2w2qwCGVDiPE953wEuuY91",
	  "5JQ",
	  "AcHNyrwe7gzGqbiAg8mouLU",
	  "8Q0B5xWQ46wBOX0AKG8xBbENDHimLD",
	  "B4Ho3FqHKXdM4BIzPuIkTNhJ0",
	  "Tq6fYbU3TTRw"
	]

### Attempt to add a malformed filter

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {"key" : "value"}}, "expr" : {"not" : {"union" : [{"var" : "9hn", "value" : "bad value"}, {"var" : "H9Bf", "set" : ["7tY", "YPdZ"]}]}}}' 'http://1lv11lamb01.nrel.gov/models/JCF/filters'
	
	value incompatible with domain

### Add a filter

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {"key" : "value"}}, "expr" : {"not" : {"union" : [{"var" : "9hn", "value" : "IJ2"}, {"var" : "H9Bf", "set" : ["7tY", "YPdZ"]}]}}}' 'http://1lv11lamb01.nrel.gov:8090/models/JCF/filters'
	
	{
	  "expr":{
	    "not":{
	      "union":[
	        {"var":"9hn","value":"IJ2"},
	        {"set":["7tY","YPdZ"],"var":"H9Bf"}
	      ]
	    }
	  },
	  "meta":{
	    "name":"test filter",
	    "filter_id":"HvuzvDYS",
	    "tags":{"key":"value"}
	  }
	}

### Retrieve a filter.

	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/filters?key="value"'
	
	["HvuzvDYS"]
	
	$ curl -H GET 'http://1lv11lamb01.nrel.gov:8090/models/JCF/filters/HvuzvDYS'
	
	{
	  "expr":{
	    "not":{
	      "union":[
	        {"var":"9hn","value":"IJ2"},
	        {"set":["7tY","YPdZ"],"var":"H9Bf"}
	      ]
	    }
	  },
	  "meta":{
	    "name":"test filter",
	    "filter_id":"HvuzvDYS",
	    "tags":{"key":"value"}
	  }
	}
