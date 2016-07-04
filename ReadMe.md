# Example usage of CESDS API

Example usage of CESDS Record API, commit de659ed7be4c23223a661e6d60d27a4aa9045ee1 of http://github.nrel.gov:kgruchal/cesds.

## Get server status

$ curl -H GET 'http://localhost:8090/server'

{"status":"ok","server_id":"20tt2hAu","models":[],"version":17}

## Attempt a request with malformed JSON

$ curl -H POST --data-ascii '{"command" : "restart}' 'http://localhost:8090/server'

{"api_error":"illegal JSON: Error in $: not enough input"}

## Attempt a request with a missing parameter

$ curl -H POST --data-ascii '{"command" : "restart"}' 'http://localhost:8090/server'

{"api_error":"illegal JSON: Error in $: key \"param\" not present"}

## Experience server failure

$ curl -H POST --data-ascii '{"command" : "restart", "param" : []}' 'http://localhost:8090/server'

{"result":"failed","additional":"random failure for testing"}

## Restart the server

$ curl -H POST --data-ascii '{"command" : "restart", "param" : []}' 'http://localhost:8090/server'

{}

$ curl -H GET 'http://localhost:8090/server'

{"status":"on_fire","server_id":"kIPVnUUmQHYC5yT","models":["zOH8","y","t"],"version":2}

## Retrieve model information

$ curl -H GET 'http://localhost:8090/server/y'

{"primary_key":"iyIl","variables":[{"domain":{"set":{"options":["Do","Kym","tMU"]}},"is_input":true,"var_id":"mj","disp":{"shortlabel":"e","label":"9z"}},{"domain":{"interval":{"bounds":[null,null]}},"is_input":false,"var_id":"iyIl","units":{"scale":3,"SI":[2,-3,0,-4,1,2,3,-2]},"disp":{"color":"#020002","shortlabel":"A","label":"VI"}}],"model_id":"y","time_key":"iyIl","model_uri":"http://bxe","generation":15,"description":"F","label":"u2YoJLbUikkqXbeEp0rLk","tags":{"g83W":-53.02027558053175,"m5n":{"td":-2,"1g":5.491602524783761,"I63m":-4},"359K":18.11972097369426}}

## Set the strategy for running simulations

$ curl -H POST --data-ascii '{"command" : "model_strat_fifo", "param" : []}' 'http://localhost:8090/server/y'

{"result":"failed","additional":"random failure for testing"}

$ curl -H: []}' 'http://localhost:8090/server/y'_strat_fifo", "param" : []}' 'http://localhost:8090/server/y'

{}

## Retrieve records

$ curl -H GET 'http://localhost:8090/server/y/records'

[{"mj":"Kym","iyIl":5.383744935322579}]

## Attempt to submit malformed work

$ curl -H POST --data-ascii '{"explicit" : {"mj" : "z"}, "random" : ["iyIl"]}' 'http://localhost:8090/server/y/work'

{"api_error":"value incompatible with domain"}

## Submit work

$ curl -H GET 'http://localhost:8090/server/y/work'

[{"status":"success","work_id":"XKAzHCYTyrIvylAVjuQRKg","result_id":"rT6y1XjaVfu"},{"status":"success","work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE","result_id":"XLW4BKeij0M2MYrOIADKdkzb"},{"status":"pending","work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"}]

## List work and watch for it to complete

$ curl -H GET 'http://localhost:8090/server/y/work'

[{"status":"success","work_id":"XKAzHCYTyrIvylAVjuQRKg","result_id":"rT6y1XjaVfu"},{"status":"success","work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE","result_id":"XLW4BKeij0M2MYrOIADKdkzb"},{"status":"pending","work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"}]

$ curl -H GET 'http://localhost:8090/server/y/work'

[{"status":"success","work_id":"XKAzHCYTyrIvylAVjuQRKg","result_id":"rT6y1XjaVfu"},{"status":"success","work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE","result_id":"XLW4BKeij0M2MYrOIADKdkzb"},{"status":"running","work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"}]

$ curl -H GET 'http://localhost:8090/server/y/work'

[{"status":"success","work_id":"XKAzHCYTyrIvylAVjuQRKg","result_id":"rT6y1XjaVfu"},{"status":"success","work_id":"zVwYAEwUIT724Omd5HzPIybPQV1NuE","result_id":"XLW4BKeij0M2MYrOIADKdkzb"},{"status":"success","work_id":"GDNq6kTVMdOd8080yaVstVCWIq1sGG"

## Check work

$ curl -H GET 'http://localhost:8090/server/y/work?work_id=XKAzHCYTyrIvylAVjuQRKg'

[{"status":"success","work_id":"XKAzHCYTyrIvylAVjuQRKg","result_id":"rT6y1XjaVfu"}]

## Retrieve results

$ curl -H GET 'http://localhost:8090/server/y/records?result_id=rT6y1XjaVfu'

[{"mj":"Do","iyIl":-18.174549356827406}]

## Attempt to submit work with duplicate primary keys

$ curl -H POST --data-ascii '{"explicit" : {"iyIl" : 20}, "random" : []}' 'http://localhost:8090/server/y/work'

{"work_id":"fq9wPOsljoA4yHIUykIbHBrMW","generation":17}

$ curl -H POST --data-ascii '{"explicit" : {"iyIl" : 20}, "random" : []}' 'http://localhost:8090/server/y/work'

{"api_error":"primary key violation"}

## Query work and results

$ curl -H GET 'http://localhost:8090/server/y/work?from=17'

[{"status":"running","work_id":"fq9wPOsljoA4yHIUykIbHBrMW"}]

$ curl -H GET 'http://localhost:8090/server/y/work?from=17'

[{"status":"success","work_id":"fq9wPOsljoA4yHIUykIbHBrMW","result_id":"wWKVjbkcuHQvNQChvK20dr4xNdTG"}]

$ curl -H GET 'http://localhost:8090/server/y/records?from=17'

[{"mj":"Kym","iyIl":20}]

## List bookmark metadata

$ curl -H GET 'http://localhost:8090/server/y/bookmark_metas'

[{"meta":{"color":"#718c12","size":2,"name":"J8dZ46sxrHY","bookmark_id":"6jF77FlBrwP5Q","tags":{"LyJE":true,"me":{"MH":true,"mWqy":false,"5":"gcsw"},"Cu5":null}}},{"meta":{"color":"#67202d","size":1,"name":"Am3wYW3dTAGP49Hx","bookmark_id":"9BZGLb3JsOfQ9KBrdvWisj53","tags":{"Vwi":[],"kTo":false,"Dgpx":3.36963399215058,"M68":-3}}},{"meta":{"color":"#2715cb","size":1,"name":"7MTV8q0OwsX6TVkWRCB07","bookmark_id":"dEQqTvRqnzylf","tags":{"rKD":"unQB"}}},{"meta":{"color":"#6cb71d","size":1,"name":"ovwP13vu8zwAUuTk2Qj","bookmark_id":"MLO","tags":{"CN":true,"HzAH":[],"py":null}}},{"meta":{"size":2,"name":"1DD8zA","bookmark_id":"J8NMG","tags":{"a":[[13.078064454308052,{"Buc":["OoGJ",["I",[{}]],3],"KL0J":false},"N9",[[],2.888946070627729]],-3,"W",[[null,"h",-1,true],"FQ"]],"ochj":[null,5.147291456179114]}}}]

## List bookmarks

$ curl -H GET 'http://localhost:8090/server/y/bookmarks'

[{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb","yREZOm1CPlpbt5MdFyZ3"],"meta":{"color":"#718c12","size":2,"name":"J8dZ46sxrHY","bookmark_id":"6jF77FlBrwP5Q","tags":{"LyJE":true,"me":{"MH":true,"mWqy":false,"5":"gcsw"},"Cu5":null}}},{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb"],"meta":{"color":"#67202d","size":1,"name":"Am3wYW3dTAGP49Hx","bookmark_id":"9BZGLb3JsOfQ9KBrdvWisj53","tags":{"Vwi":[],"kTo":false,"Dgpx":3.36963399215058,"M68":-3}}},{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb"],"meta":{"color":"#2715cb","size":1,"name":"7MTV8q0OwsX6TVkWRCB07","bookmark_id":"dEQqTvRqnzylf","tags":{"rKD":"unQB"}}},{"record_ids":["yREZOm1CPlpbt5MdFyZ3"],"meta":{"color":"#6cb71d","size":1,"name":"ovwP13vu8zwAUuTk2Qj","bookmark_id":"MLO","tags":{"CN":true,"HzAH":[],"py":null}}},{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb","yREZOm1CPlpbt5MdFyZ3"],"meta":{"size":2,"name":"1DD8zA","bookmark_id":"J8NMG","tags":{"a":[[13.078064454308052,{"Buc":["OoGJ",["I",[{}]],3],"KL0J":false},"N9",[[],2.888946070627729]],-3,"W",[[null,"h",-1,true],"FQ"]],"ochj":[null,5.147291456179114]}}}]

$ curl -H GET 'http://localhost:8090/server/y/bookmarks?bookmark_id=6jF77FlBrwP5Q'

[{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb","yREZOm1CPlpbt5MdFyZ3"],"meta":{"color":"#718c12","size":2,"name":"J8dZ46sxrHY","bookmark_id":"6jF77FlBrwP5Q","tags":{"LyJE":true,"me":{"MH":true,"mWqy":false,"5":"gcsw"},"Cu5":null}}}]

## Search bookmarks by tag

$ curl -H GET 'http://localhost:8090/server/y/bookmark_metas?LyJE=true'

[{"meta":{"color":"#718c12","size":2,"name":"J8dZ46sxrHY","bookmark_id":"6jF77FlBrwP5Q","tags":{"LyJE":true,"me":{"MH":true,"mWqy":false,"5":"gcsw"},"Cu5":null}}}]

## Add a bookmark

$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "size" : 1, "tags" : {}}, "record_ids" : ["XLW4BKeij0M2MYrOIADKdkzb"]}' 'http://localhost:8090/server/y/bookmarks'

{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb"],"meta":{"size":1,"name":"test bookmark","bookmark_id":"2JItRD3jfSGspm5yAnud8d5","tags":{}}}

## Retrieve a bookmark

$ curl -H GET 'http://localhost:8090/server/y/bookmarks?bookmark_id=2JItRD3jfSGspm5yAnud8d5'

[{"record_ids":["XLW4BKeij0M2MYrOIADKdkzb"],"meta":{"size":1,"name":"test bookmark","bookmark_id":"2JItRD3jfSGspm5yAnud8d5","tags":{}}}]

## Attempt to add a bookmark incorrectly referencing records

$ curl -H POST --data-ascii '{"meta" : {"name" : "test bookmark", "size" : 1, "tags" : {}}, "record_ids" : ["XLW4BKeij0M2MYrOIADxKdkzb"]}' 'http://localhost:8090/server/y/bookmarks'

{"api_error":"invalid record identifiers"}

## List filter metadata

$ curl -H GET 'http://localhost:8090/server/y/filter_metas'

[{"meta":{"color":"#d8560a","size":-24,"name":"pcqqzoTA5","filter_id":"7W5","tags":{"LFQ":null,"A40":null,"ZI":false,"8kM":null}}},{"meta":{"color":"#0b2cf3","size":21,"name":"2Hd","filter_id":"GWL","tags":{"cg":"T0","O27":{},"8Z":4,"8nMV":true}}},{"meta":{"color":"#2ca686","size":-30,"name":"XXKEy","filter_id":"lsWOGGGAQE8SRgssdB","tags":{"oI":1,"717":"FUZ","Y8":[{}]}}},{"meta":{"color":"#09e29c","name":"eVkfA0dFhS9gZaE","filter_id":"VXRjPEFa4zedeHZLKILhSuUQvd","tags":{"jHVt":null,"O":-4}}},{"meta":{"color":"#5cf0c1","size":-5,"name":"RGGvoF7gmr","filter_id":"oqU","tags":{"1":"Rw3y","gKL":{"iRTp":[true,{"A":4}],"mat":"N","FS":false}}}},{"meta":{"color":"#51758e","size":20,"name":"hC","filter_id":"gsIJJJqwzt2DQ","tags":{"M":"K","PpQg":"PRUh","no3k":true,"8n":true}}},{"meta":{"color":"#129fa3","size":-22,"name":"wWRWe34Ntwz","filter_id":"QoViW","tags":{"k3SK":[{"m":"3"},null],"S7R":"VWju","UB":true,"BkS":"yuoq"}}},{"meta":{"color":"#5f1254","size":-29,"name":"FFtkmlHsXHyWgj","tags":{}}},{"meta":{"color":"#5d24c6","size":-8,"name":"cHh3a9X8wWv8","filter_id":"XxKGSve8FLc","tags":{"P2":"XwVJ"}}},{"meta":{"size":-15,"name":"tKj7mfXKHxRXnBMDN","filter_id":"z","tags":{"jbx":{"z":{"tvx":true,"WNb":4,"a":-7.719473643594483,"PE":{"W":false,"BMf2":-1,"qdhz":"z"}},"c9":null,"eIIM":1,"V":[-2]},"3KWo":"cv","0u":[null,{},{"EC":null,"Zt":-4.632387197741746,"BP":null},[{},null,"Sjs",{}]],"Y":"nf"}}},{"meta":{"name":"eY1kSTEj3coXy","filter_id":"C9M6rSWT4Ry1PC8H2CO5rb66","tags":{"P":[false,"02ox",{"b":4,"riV":[null],"TB35":{"SY77":{"2ZP5":1,"SyZ":"A16","3ofD":"9"},"Miis":4,"5":false,"53Ns":"r0qo"},"ba":"IDH"},[]],"SB1T":{"p":false,"JR5n":true,"k":true}}}},{"meta":{"color":"#2129e8","size":-10,"name":"lwamK4jiBpuBVB","filter_id":"7XEsy","tags":{"WRA":null,"dL":-3.4478323918148694}}},{"meta":{"color":"#95fbd9","size":11,"name":"xoZmHT9HUaJbEBmsAZSFJORDNcn","filter_id":"5tst","tags":{}}},{"meta":{"size":-23,"name":"ZMShhvVljt7iimFFN2Krjj2mV4","filter_id":"C","tags":{"Vfem":["WG"],"5f":2,"1":false,"ml":3}}},{"meta":{"color":"#d98f1c","size":25,"name":"TJwsd6E1wwefwNMcqI0V4j2","filter_id":"E5wgLMiITxhuJN","tags":{"gV":"UX"}}},{"meta":{"size":7,"name":"lLPDUxIest72xV90xqrwrLy","filter_id":"fc","tags":{"oJ":"OWpF","5AB":-6.716711321147531,"YE4Q":["EOve",null],"UOF":false}}},{"meta":{"size":-24,"name":"Jq6HgKNonGC1mMZ5oeOJ480hX","filter_id":"xe3mTkszBLCOinntC","tags":{"yiz":"HDc"}}},{"meta":{"color":"#b0aa53","size":-4,"name":"D5WcjlHFqEKkimbr26Vq3qKpHm","filter_id":"9","tags":{"4":0}}}]

## Retrieve a filter

$ curl -H GET 'http://localhost:8090/server/y/filters?filter_id=7W5'

[{"meta":{"color":"#d8560a","size":-24,"name":"pcqqzoTA5","filter_id":"7W5","tags":{"LFQ":null,"A40":null,"ZI":false,"8kM":null}}}]

## Attempt to add a malformed filter

$ curl -H POST --data-ascii '{"meta" : {"filter_id" : "test", "name" : "test filter", "tags" : {}}, "expr" : {"var" : "iyIl", "set" : ["Do", "tMU"]}}' 'http://localhost:8090/server/y/filters'

{"api_error":"incompatible domains"}

$ curl -H POST --data-ascii '{"meta" : {"filter_id" : "test", "name" : "test filter", "tags" : {}}, "expr" : {"var" : "iyIl", "value" : "tMU"}}' 'http://localhost:8090/server/y/filters'

{"api_error":"value not in domain"}

## Add a filter

$ curl -H POST --data-ascii '{"meta" : {"name" : "test filter", "tags" : {}}, "expr" : {"expr" : "union", "a" : {"var" : "mj", "set" : ["Do", "tMU"]}, "b" : {"var" : "iyIl", "interval" : [-4, null]}}}' 'http://localhost:8090/server/y/filters'

{"expr":{"expr":"union","a":{"set":["Do","tMU"],"var":"mj"},"b":{"var":"iyIl","interval":[-4,null]}},"meta":{"name":"test filter","filter_id":"eDscdQiLrIuZV3tNVZP25VK51Wr","tags":{}}}
