#/bin/bash

curl -X GET 'localhost:8091/'                                                    --output test-01.out
curl -X GET 'localhost:8091/models/RSF2v0'                                       --output test-02.out
curl -X GET 'localhost:8091/models/RSF2v0/records?from=1468640300&to=1468640960' --output test-03.out
