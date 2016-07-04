# CESDS Records API in Haskell

## Overview

This [Haskell package](cesds-records.cabal) contains . . .

1.  a library for working with the CESDS Records API,
2.  [a skeletal server](src/CESDS/Server.hs) for the REST API
3.  [automated tests](src/TestJSON.hs) for the library, and
4.  [an example server](src/Main.hs) that implements the REST API and provides random, but consistent data and validates input.

This implementation comforms to http://github.nrel.gov:kgruchal/cesds/commit/de659ed7be4c23223a661e6d60d27a4aa9045ee1.

## Example usage of CESDS API

### Get server status

	$ curl -H GET 'http://localhost:8090/server'
	
	{  
	  "status":"ok",
	  "server_id":"20tt2hAu",
	  "models":[  
	
	  ],
	  "version":17
	}

### Attempt a request with malformed JSON

	$ curl -H POST --data-ascii '{"command" : "restart}' 'http://localhost:8090/server'
	
	{  
	  "api_error":"illegal JSON: Error in $: not enough input"
	}

### Attempt a request with a missing parameter

	$ curl -H POST --data-ascii '{"command" : "restart"}' 'http://localhost:8090/server'
	
	{  
	  "api_error":"illegal JSON: Error in $: key \"param\" not present"
	}

### Experience server failure

	$ curl -H POST --data-ascii '{"command" : "restart", "param" : []}' 'http://localhost:8090/server'
	
	{  
	  "result":"failed",
	  "additional":"random failure for testing"
	}

### Restart the server

	$ curl -H POST --data-ascii '{"command" : "restart", "param" : []}' 'http://localhost:8090/server'
	
	{}
	
	$ curl -H GET 'http://localhost:8090/server'
	
	{  
	  "status":"on_fire",
	  "server_id":"kIPVnUUmQHYC5yT",
	  "models":[  
	    "zOH8",
	    "y",
	    "t"
	  ],
	  "version":2
	}

### Retrieve model information

	$ curl -H GET 'http://localhost:8090/server/y'
	
	{  
	  "primary_key":"iyIl",
	  "variables":[  
	    {  
	      "domain":{  
	        "set":{  
	          "options":[  "Do", "Kym", "tMU" ]
	        }
	      },
	      "is_input":true,
	      "var_id":"mj",
	      "disp":{  
	        "shortlabel":"e",
	        "label":"9z"
	      }
	    },
	    {  
	      "domain":{  
	        "interval":{  
	          "bounds":[  null, null ]
	        }
	      },
	      "is_input":false,
	      "var_id":"iyIl",
	      "units":{  
	        "scale":3,
	        "SI":[  2, -3, 0, -4, 1, 2, 3, -2 ]
	      },
	      "disp":{  
	        "color":"#020002",
	        "shortlabel":"A",
	        "label":"VI"
	      }
	    }
	  ],
	  "model_id":"y",
	  "time_key":"iyIl",
	  "model_uri":"http://bxe",
	  "generation":15,
	  "description":"F",
	  "label":"u2YoJLbUikkqXbeEp0rLk",
	  "tags":{  
	    "g83W":-53.02027558053175,
	    "m5n":{  
	      "td":-2,
	      "1g":5.491602524783761,
	      "I63m":-4
	    },
	    "359K":18.11972097369426
	  }
	}

### Set the strategy for running simulations

	$ curl -H POST --data-ascii '{"command" : "model_strat_fifo", "param" : []}' 'http://localhost:8090/server/y'
	
	{  
	  "result":"failed",
	  "additional":"random failure for testing"
	}
	
	$ curl -H: []}' 'http://localhost:8090/server/y'_strat_fifo", "param" : []}' 'http://localhost:8090/server/y'
	
	{}

### Retrieve records

	$ curl -H GET 'http://localhost:8090/server/y/records'
	
	[  
	  {  
	    "mj":"Kym",
	    "iyIl":5.383744935322579
	  }
	]

### Attempt to submit malformed work

	$ curl -H POST --data-ascii '{"explicit" : {"mj" : "z"}, "random" : ["iyIl"]}' 'http://localhost:8090/server/y/work'
	
	{  
	  "api_error":"value incompatible with domain"
	}

### List work and watch for it to complete

	$ curl -H GET 'http://localhost:8090/server/y/work'
	
	[  
	  {  
	    "status":"success",
	    "work_id":"XKAzHCYTyrIvylAVjuQRKg",
	    "result_id":"rT6y1XjaVfu"
	  },
	  {  
	    "status":"success",
	    "work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE",
	    "result_id":"XLW4BKeij0M2MYrOIADKdkzb"
	  },
	  {  
	    "status":"pending",
	    "work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"
	  }
	]
	
	$ curl -H GET 'http://localhost:8090/server/y/work'
	
	[  
	  {  
	    "status":"success",
	    "work_id":"XKAzHCYTyrIvylAVjuQRKg",
	    "result_id":"rT6y1XjaVfu"
	  },
	  {  
	    "status":"success",
	    "work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE",
	    "result_id":"XLW4BKeij0M2MYrOIADKdkzb"
	  },
	  {  
	    "status":"running",
	    "work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"
	  }
	]
	
	$ curl -H GET 'http://localhost:8090/server/y/work'
	
	[  
	  {  
	    "status":"success",
	    "work_id":"XKAzHCYTyrIvylAVjuQRKg",
	    "result_id":"rT6y1XjaVfu"
	  },
	  {  
	    "status":"success",
	    "work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE",
	    "result_id":"XLW4BKeij0M2MYrOIADKdkzb"
	  },
	  {  
	    "status":"success",
	    "work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG",
	    "result_id":"aKSnAUu77dz"
	  }
	]

### Retrieve results

	$ curl -H GET 'http://localhost:8090/server/y/records?result_id=rT6y1XjaVfu'
	
	[  
	  {  
	    "mj":"Do",
	    "iyIl":-18.174549356827406
	  }
	]

### Submit work

	$ curl -H POST --data-ascii '{"explicit" : {"iyIl" : 20}, "random" : []}' 'http://localhost:8090/server/y/work'
	
	{  
	  "work_id":"fq9wPOsljoA4yHIUykIbHBrMW",
	  "generation":17
	}

### Attempt to submit work with duplicate primary keys

	$ curl -H POST --data-ascii '{"explicit" : {"iyIl" : 20}, "random" : []}' 'http://localhost:8090/server/y/work'
	
	{  
	  "api_error":"primary key violation"
	}

### Query work and results

	$ curl -H GET 'http://localhost:8090/server/y/work?from=17&to=1000'
	
	[  
	  {  
	    "status":"running",
	    "work_id":"fq9wPOsljoA4yHIUykIbHBrMW"
	  }
	]
	
	$ curl -H GET 'http://localhost:8090/server/y/work?from=17'
	
	[  
	  {  
	    "status":"success",
	    "work_id":"fq9wPOsljoA4yHIUykIbHBrMW",
	    "result_id":"wWKVjbkcuHQvNQChvK20dr4xNdTG"
	  }
	]
	
	$ curl -H GET 'http://localhost:8090/server/y/records?from=17'
	
	[  
	  {  
	    "mj":"Kym",
	    "iyIl":20
	  }
	]

### List bookmark metadata

	$ curl -H GET 'http://localhost:8090/server/y/bookmark_metas'
	
	[  
	  {  
	    "meta":{  
	      "color":"#718c12",
	      "size":2,
	      "name":"J8dZ46sxrHY",
	      "bookmark_id":"6jF77FlBrwP5Q",
	      "tags":{  
	        "LyJE":true,
	        "me":{  
	          "MH":true,
	          "mWqy":false,
	          "5":"gcsw"
	        },
	        "Cu5":null
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#67202d",
	      "size":1,
	      "name":"Am3wYW3dTAGP49Hx",
	      "bookmark_id":"9BZGLb3JsOfQ9KBrdvWisj53",
	      "tags":{  
	        "Vwi":[  ],
	        "kTo":false,
	        "Dgpx":3.36963399215058,
	        "M68":-3
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#2715cb",
	      "size":1,
	      "name":"7MTV8q0OwsX6TVkWRCB07",
	      "bookmark_id":"dEQqTvRqnzylf",
	      "tags":{  
	        "rKD":"unQB"
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#6cb71d",
	      "size":1,
	      "name":"ovwP13vu8zwAUuTk2Qj",
	      "bookmark_id":"MLO",
	      "tags":{  
	        "CN":true,
	        "HzAH":[  ],
	        "py":null
	      }
	    }
	  }
	]

### List bookmarks

	$ curl -H GET 'http://localhost:8090/server/y/bookmarks'
	
	[  
	  {  
	    "record_ids":[  
	      "XLW4BKeij0M2MYrOIADKdkzb",
	      "yREZOm1CPlpbt5MdFyZ3"
	    ],
	    "meta":{  
	      "color":"#718c12",
	      "size":2,
	      "name":"J8dZ46sxrHY",
	      "bookmark_id":"6jF77FlBrwP5Q",
	      "tags":{  
	        "LyJE":true,
	        "me":{  
	          "MH":true,
	          "mWqy":false,
	          "5":"gcsw"
	        },
	        "Cu5":null
	      }
	    }
	  },
	  {  
	    "record_ids":[  
	      "XLW4BKeij0M2MYrOIADKdkzb"
	    ],
	    "meta":{  
	      "color":"#67202d",
	      "size":1,
	      "name":"Am3wYW3dTAGP49Hx",
	      "bookmark_id":"9BZGLb3JsOfQ9KBrdvWisj53",
	      "tags":{  
	        "Vwi":[  ],
	        "kTo":false,
	        "Dgpx":3.36963399215058,
	        "M68":-3
	      }
	    }
	  },
	  {  
	    "record_ids":[  
	      "XLW4BKeij0M2MYrOIADKdkzb"
	    ],
	    "meta":{  
	      "color":"#2715cb",
	      "size":1,
	      "name":"7MTV8q0OwsX6TVkWRCB07",
	      "bookmark_id":"dEQqTvRqnzylf",
	      "tags":{  
	        "rKD":"unQB"
	      }
	    }
	  },
	  {  
	    "record_ids":[  
	      "yREZOm1CPlpbt5MdFyZ3"
	    ],
	    "meta":{  
	      "color":"#6cb71d",
	      "size":1,
	      "name":"ovwP13vu8zwAUuTk2Qj",
	      "bookmark_id":"MLO",
	      "tags":{  
	        "CN":true,
	        "HzAH":[  ],
	        "py":null
	      }
	    }
	  }
	]

### Search bookmarks by tag

	$ curl -H GET 'http://localhost:8090/server/y/bookmark_metas?LyJE=true'
	
	[  
	  {  
	    "meta":{  
	      "color":"#718c12",
	      "size":2,
	      "name":"J8dZ46sxrHY",
	      "bookmark_id":"6jF77FlBrwP5Q",
	      "tags":{  
	        "LyJE":true,
	        "me":{  
	          "MH":true,
	          "mWqy":false,
	          "5":"gcsw"
	        },
	        "Cu5":null
	      }
	    }
	  }
	]

### Add a bookmark

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "size" : 1, "tags" : {}}, "record_ids" : ["XLW4BKeij0M2MYrOIADKdkzb"]}' 'http://localhost:8090/server/y/bookmarks'
	
	{  
	  "record_ids":[  
	    "XLW4BKeij0M2MYrOIADKdkzb"
	  ],
	  "meta":{  
	    "size":1,
	    "name":"test bookmark",
	    "bookmark_id":"2JItRD3jfSGspm5yAnud8d5",
	    "tags":{  
	    }
	  }
	}

### Retrieve a bookmark

	$ curl -H GET 'http://localhost:8090/server/y/bookmarks?bookmark_id=2JItRD3jfSGspm5yAnud8d5'
	
	[  
	  {  
	    "record_ids":[  
	      "XLW4BKeij0M2MYrOIADKdkzb"
	    ],
	    "meta":{  
	      "size":1,
	      "name":"test bookmark",
	      "bookmark_id":"2JItRD3jfSGspm5yAnud8d5",
	      "tags":{  
	
	      }
	    }
	  }
	]

### Attempt to add a bookmark incorrectly referencing records

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "size" : 1, "tags" : {}}, "record_ids" : ["XLW4BKeij0M2MYrOIADxKdkzb"]}' 'http://localhost:8090/server/y/bookmarks'
	
	{  
	  "api_error":"invalid record identifiers"
	}

### List filter metadata

	$ curl -H GET 'http://localhost:8090/server/y/filter_metas'
	
	[  
	  {  
	    "meta":{  
	      "color":"#d8560a",
	      "size":-24,
	      "name":"pcqqzoTA5",
	      "filter_id":"7W5",
	      "tags":{  
	        "LFQ":null,
	        "A40":null,
	        "ZI":false,
	        "8kM":null
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#0b2cf3",
	      "size":21,
	      "name":"2Hd",
	      "filter_id":"GWL",
	      "tags":{  
	        "cg":"T0",
	        "O27":{  
	
	        },
	        "8Z":4,
	        "8nMV":true
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#2ca686",
	      "size":-30,
	      "name":"XXKEy",
	      "filter_id":"lsWOGGGAQE8SRgssdB",
	      "tags":{  
	        "oI":1,
	        "717":"FUZ",
	        "Y8":[  
	          {  }
	        ]
	      }
	    }
	  },
	  {  
	    "meta":{  
	      "color":"#09e29c",
	      "name":"eVkfA0dFhS9gZaE",
	      "filter_id":"VXRjPEFa4zedeHZLKILhSuUQvd",
	      "tags":{  
	        "jHVt":null,
	        "O":-4
	      }
	    }
	  }
	]

### Attempt to add a malformed filter

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {}}, "expr" : {"var" : "iyIl", "set" : ["Do", "tMU"]}}' 'http://localhost:8090/server/y/filters'
	
	{  
	  "api_error":"incompatible domains"
	}
	
	$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {}}, "expr" : {"var" : "iyIl", "value" : "tMU"}}' 'http://localhost:8090/server/y/filters'
	
	{  
	  "api_error":"value not in domain"
	}

### Add a filter

	$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {}}, "expr" : {"expr" : "union", "a" : {"var" : "mj", "set" : ["Do", "tMU"]}, "b" : {"var" : "iyIl", "interval" : [-4, null]}}}' 'http://localhost:8090/server/y/filters'
	
	{  
	  "expr":{  
	    "expr":"union",
	    "a":{  
	      "set":[  
	        "Do",
	        "tMU"
	      ],
	      "var":"mj"
	    },
	    "b":{  
	      "var":"iyIl",
	      "interval":[  
	        -4,
	        null
	      ]
	    }
	  },
	  "meta":{  
	    "name":"test filter",
	    "filter_id":"eDscdQiLrIuZV3tNVZP25VK51Wr",
	    "tags":{  
	
	    }
	  }
	}
