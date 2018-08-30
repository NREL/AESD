SHELL=bash
BROWSERIFY=node_modules/.bin/browserify


all: aesd-bundle.js

clean:
	-rm aesd-bundle.js gen/records_def_4_pb.js


serve: all
	./node_modules/.bin/http-server

setup-tools:
	npm install browserify

gen/%_pb.js: %.proto
	protoc --js_out=import_style=commonjs,binary:gen $^

aesd-bundle.js: src/standalone.js src/aesd-records.js gen/records_def_4_pb.js lib/google-protobuf.js
	$(BROWSERIFY) --outfile $@ $^


.SUFFIXES:
