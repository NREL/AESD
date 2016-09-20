#/bin/bash

####SERVER=http://1lv11lamb01.nrel.gov:8091
SERVER=http://localhost:8090
SENSOR=@1edb6d30-f64869a4
START=1473536000
FINISH=1473537552

curl --silent --show-error -X GET $SERVER/                                                --output test-01.out
curl --silent --show-error -X GET $SERVER/models/$SENSOR/                                 --output test-02.out
curl --silent --show-error -X GET $SERVER/models/$SENSOR/records\?from=$START\&to=$FINISH --output test-03.out
curl --silent --show-error -X GET $SERVER/models/$SENSOR/records\?from=$START\&to=$FINISH --output test-03.out
