#!/bin/bash

export url=ws://insightz.hpc.nrel.gov:50375
export model_id=58698fab-bba4-5ca7-8e5b-47c743dc43e9

python -m Records.cli request_records_data --server_url=${url} --model_id=${model_id}
