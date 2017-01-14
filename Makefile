SHELL=bash
BROWSERIFY=node_modules/.bin/browserify


all: bundle.js

clean:
	-rm bundle.js gen/records_def_3_pb.js


setup-tools:
	npm install browserify

gen/%_pb.js: %.proto
	protoc --js_out=import_style=commonjs,binary:gen $^

bundle.js: src/standalone.js src/cesds-records.js gen/records_def_3_pb.js lib/google-protobuf.js
	$(BROWSERIFY) --outfile $@ $^


.SUFFIXES:
