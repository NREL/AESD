# CESDS Records from Haystack


## Example Session


### Find the name of the model

	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/'
	
	{  
	   "status":{  
	      "state":"ok",
	      "message":"operating normally"
	   },
	   "server_id":"CESDS Haystack",
	   "server_type":"record_server",
	   "models":[  
	      "RSF2v0"
	   ],
	   "version":1
	}

### Retrieve the model metadata

	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0'
	
	{  
	   "record_id_var":"time",
	   "variables":[  
	      {  
	         "display":{  
	            "shortlabel":"Time",
	            "label":"Time Stamp"
	         },
	         "domain":{  "set":{  "options":[  ] } },
	         "is_input":false,
	         "var_id":"time"
	      },
	      {  
	         "display":{  
	            "shortlabel":"Seconds",
	            "label":"POSIX Seconds"
	         },
	         "domain":{  "interval":{  "bounds":[  315558000, null ] } },
	         "is_input":false,
	         "var_id":"epoch",
	         "units":{  "scale":1, "SI":[  0, 0, 1, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-d9847159",
	            "label":"RSF2 PV Generation Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-d9847159",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-ce98398c",
	            "label":"RSF2 Plug Loads Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-ce98398c",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-bcf051ea",
	            "label":"RSF2 Panel SWB Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-bcf051ea",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-a9d7ca09",
	            "label":"RSF2 Panel H2 Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-a9d7ca09",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-f6668673",
	            "label":"RSF2 Occupant Elevator Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-f6668673",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-9a89271e",
	            "label":"RSF2 Mechanical Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-9a89271e",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-f64869a4",
	            "label":"RSF2 Main Meter Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-f64869a4",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-6e697a28",
	            "label":"RSF2 Lighting Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-6e697a28",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-a4ccaaf4",
	            "label":"RSF2 Freight Elevator Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-a4ccaaf4",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-db52fab5",
	            "label":"RSF2 Emergency Lighting Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-db52fab5",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-fa21e31d",
	            "label":"RSF2 Elevators Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-fa21e31d",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-1e909c64",
	            "label":"RSF2 Building Load Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-1e909c64",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-5d812ce6",
	            "label":"RSF2 AC-3 Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-5d812ce6",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-60aa00e5",
	            "label":"RSF2 AC-2/2a Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-60aa00e5",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      },
	      {  
	         "display":{  
	            "shortlabel":"@1edb6d30-73828443",
	            "label":"RSF2 AC-1 Power"
	         },
	         "domain":{  "interval":{  "bounds":[  null, null ] } },
	         "is_input":false,
	         "var_id":"@1edb6d30-73828443",
	         "units":{  "scale":1000, "SI":[  2, 1, -3, 0, 0, 0, 0, 0 ] }
	      }
	   ],
	   "model_id":"RSF2v0",
	   "time_key":"epoch",
	   "model_uri":"http://www.nrel.gov/sustainable_nrel/rsf.html#RSF2v0",
	   "generation":1468640371,
	   "description":"Selected power meters from the RSF 2",
	   "label":"RSF 2 Version 0",
	   "record_count":0,
	   "tags":{  
	      "DC.source":"https://skyspark-ops.nrel.gov/proj/nrel",
	      "DC.description":"Selected power meters from the RSF 2",
	      "DC.creator":"Brian W Bush <brian.bush@nrel.gov"
	   }
	}

### Retreive the records currently cached by the server

	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0/records'
	
	[]

Retrieving an empty result just means that no records are cached.  To pull records from SkySpark, we need to specify a starting time and (optionally) a finishing time.  A starting time is required because without it the voluminous whole history of measurements would be returned.

The generation number is simple the Unix Epoch time in seconds.  Using that as the key makes it easy to select times of interest that have not already been cached on the server.

	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0/records?from=1468640300&to=1468640960'
	
	[  
	   {  
	      "variables":{  
	         "@1edb6d30-1e909c64":5.2928572573832104,
	         "Time Stamp":"2016-07-15T21:39:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.822857243674142,
	         "@1edb6d30-9a89271e":0.3400000035762787,
	         "@1edb6d30-fa21e31d":0.1300000101327896
	      },
	      "id":"1468640340"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-1e909c64":5.2928572573832104,
	         "Time Stamp":"2016-07-15T21:39:25-06:00 Denver",
	         "@1edb6d30-6e697a28":4.822857243674142,
	         "@1edb6d30-9a89271e":0.3400000035762787,
	         "@1edb6d30-fa21e31d":0.1300000101327896,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640365"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-1e909c64":5.2928572573832104,
	         "Time Stamp":"2016-07-15T21:39:48-06:00 Denver",
	         "@1edb6d30-6e697a28":4.822857243674142,
	         "@1edb6d30-9a89271e":0.3400000035762787,
	         "@1edb6d30-fa21e31d":0.1300000101327896,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640388"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-1e909c64":5.2928572573832104,
	         "Time Stamp":"2016-07-15T21:39:55-06:00 Denver",
	         "@1edb6d30-6e697a28":4.822857243674142,
	         "@1edb6d30-9a89271e":0.3400000035762787,
	         "@1edb6d30-fa21e31d":0.1300000101327896,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640395"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-1e909c64":5.2705953905270215,
	         "Time Stamp":"2016-07-15T21:40:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.82142870766776,
	         "@1edb6d30-9a89271e":0.3225000128149986,
	         "@1edb6d30-fa21e31d":0.12666667004426319,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640400"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-1e909c64":5.2705953905270215,
	         "Time Stamp":"2016-07-15T21:40:49-06:00 Denver",
	         "@1edb6d30-6e697a28":4.82142870766776,
	         "@1edb6d30-9a89271e":0.3225000128149986,
	         "@1edb6d30-fa21e31d":0.12666667004426319,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640449"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-1e909c64":5.248333523670833,
	         "Time Stamp":"2016-07-15T21:41:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.820000171661377,
	         "@1edb6d30-9a89271e":0.30500002205371857,
	         "@1edb6d30-fa21e31d":0.12333332995573676,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640460"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.248333523670833,
	         "Time Stamp":"2016-07-15T21:41:07-06:00 Denver",
	         "@1edb6d30-6e697a28":4.820000171661377,
	         "@1edb6d30-9a89271e":0.30500002205371857,
	         "@1edb6d30-fa21e31d":0.12333332995573676,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640467"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.248333523670833,
	         "Time Stamp":"2016-07-15T21:41:13-06:00 Denver",
	         "@1edb6d30-6e697a28":4.820000171661377,
	         "@1edb6d30-9a89271e":0.30500002205371857,
	         "@1edb6d30-fa21e31d":0.12333332995573676,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640473"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.248333523670833,
	         "Time Stamp":"2016-07-15T21:41:17-06:00 Denver",
	         "@1edb6d30-6e697a28":4.820000171661377,
	         "@1edb6d30-9a89271e":0.30500002205371857,
	         "@1edb6d30-fa21e31d":0.12333332995573676,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640477"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.248333523670833,
	         "Time Stamp":"2016-07-15T21:41:31-06:00 Denver",
	         "@1edb6d30-6e697a28":4.820000171661377,
	         "@1edb6d30-9a89271e":0.30500002205371857,
	         "@1edb6d30-fa21e31d":0.12333332995573676,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640491"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.287500110765299,
	         "Time Stamp":"2016-07-15T21:42:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.810000101725261,
	         "@1edb6d30-9a89271e":0.3575000191728274,
	         "@1edb6d30-fa21e31d":0.11999998986721039,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640520"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.287500110765299,
	         "Time Stamp":"2016-07-15T21:42:54-06:00 Denver",
	         "@1edb6d30-6e697a28":4.810000101725261,
	         "@1edb6d30-9a89271e":0.3575000191728274,
	         "@1edb6d30-fa21e31d":0.11999998986721039,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640574"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.332500043014686,
	         "Time Stamp":"2016-07-15T21:43:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.8000000317891445,
	         "@1edb6d30-9a89271e":0.4100000162919363,
	         "@1edb6d30-fa21e31d":0.1224999949336052,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640580"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.28999999165534973,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.332500043014686,
	         "Time Stamp":"2016-07-15T21:43:17-06:00 Denver",
	         "@1edb6d30-6e697a28":4.8000000317891445,
	         "@1edb6d30-9a89271e":0.4100000162919363,
	         "@1edb6d30-fa21e31d":0.1224999949336052,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640597"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.332500043014686,
	         "Time Stamp":"2016-07-15T21:43:24-06:00 Denver",
	         "@1edb6d30-6e697a28":4.8000000317891445,
	         "@1edb6d30-9a89271e":0.4100000162919363,
	         "@1edb6d30-fa21e31d":0.1224999949336052,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640604"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.3816666305065155,
	         "Time Stamp":"2016-07-15T21:44:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":0.46666666865348816,
	         "@1edb6d30-fa21e31d":0.125,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640640"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.36000001430511475,
	         "@1edb6d30-1e909c64":5.3816666305065155,
	         "Time Stamp":"2016-07-15T21:44:19-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":0.46666666865348816,
	         "@1edb6d30-fa21e31d":0.125,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640659"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":5.3816666305065155,
	         "Time Stamp":"2016-07-15T21:44:37-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":0.46666666865348816,
	         "@1edb6d30-fa21e31d":0.125,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.820000171661377,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640677"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":5.3816666305065155,
	         "Time Stamp":"2016-07-15T21:44:43-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":0.46666666865348816,
	         "@1edb6d30-fa21e31d":0.125,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":40.0099983215332,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640683"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":5.3816666305065155,
	         "Time Stamp":"2016-07-15T21:44:46-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":0.46666666865348816,
	         "@1edb6d30-fa21e31d":0.125,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640686"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":9.083334356546402e-2,
	         "Time Stamp":"2016-07-15T21:45:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.6666661500930786e-2,
	         "@1edb6d30-fa21e31d":0.1275000050663948,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640700"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":9.083334356546402e-2,
	         "Time Stamp":"2016-07-15T21:45:01-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.6666661500930786e-2,
	         "@1edb6d30-fa21e31d":0.1275000050663948,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640701"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":0.33000001311302185,
	         "Time Stamp":"2016-07-15T21:46:00-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.0000001192092896e-2,
	         "@1edb6d30-fa21e31d":0.36000001430511475,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.3499999940395355
	      },
	      "id":"1468640760"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3499999940395355,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":0.33000001311302185,
	         "Time Stamp":"2016-07-15T21:46:23-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.0000001192092896e-2,
	         "@1edb6d30-fa21e31d":0.36000001430511475,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640783"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3400000035762787,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.30000001192092896,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":0.33000001311302185,
	         "Time Stamp":"2016-07-15T21:46:46-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.0000001192092896e-2,
	         "@1edb6d30-fa21e31d":0.36000001430511475,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640806"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3400000035762787,
	         "@1edb6d30-d9847159":0,
	         "@1edb6d30-60aa00e5":0.3100000023841858,
	         "@1edb6d30-73828443":0.5099999904632568,
	         "@1edb6d30-1e909c64":0.33000001311302185,
	         "Time Stamp":"2016-07-15T21:46:53-06:00 Denver",
	         "@1edb6d30-6e697a28":4.789999961853027,
	         "@1edb6d30-9a89271e":-3.0000001192092896e-2,
	         "@1edb6d30-fa21e31d":0.36000001430511475,
	         "@1edb6d30-f6668673":-0.23000000417232513,
	         "@1edb6d30-db52fab5":4.789999961853027,
	         "@1edb6d30-f64869a4":35.65999984741211,
	         "@1edb6d30-a4ccaaf4":0.36000001430511475
	      },
	      "id":"1468640813"
	   }
	]

The server is combining observations at different measurement times: i.e., not all variables are measured at exactly the same time.  In fact some variables are measured at high frequency, and others at low frequency. One could imagine several strategies for dealing with this within the CESDS Records API:

1.  Only report measurements at the time they were measured. This would result in a many records with missing values.
2.  At each time where a measurement has been made, report the most recently measured value of all of the variables.
3.  Use interpolation or extrapolation to align the variables to a particular time slice.

This server currently uses a variation of the second strategy: Starting just before the start of the query window, all values are missing.  Then the most recently measured value is retained in the reporting at later times.

We can also retreive the records after a certain time.

	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0/records?from=1468640800'
	
	[  
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3400000035762787,
	         "Time Stamp":"2016-07-15T21:46:46-06:00 Denver"
	      },
	      "id":"1468640806"
	   },
	   {  
	      "variables":{  
	         "@1edb6d30-5d812ce6":-0.3400000035762787,
	         "@1edb6d30-60aa00e5":0.3100000023841858,
	         "Time Stamp":"2016-07-15T21:46:53-06:00 Denver"
	      },
	      "id":"1468640813"
	   }
	]


### Clear the cache on the server.

	$ curl -X POST --data-ascii '{"command" : "clear"}' 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0/command'
	
	{"result":"server reinitialized"}
	
	$ curl -X GET 'http://1lv11lamb01.nrel.gov:8091/models/RSF2v0/records'
	
	[]
