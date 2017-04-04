SHELL=bash

PROTOC=/opt/protoc-3.0.0-linux-x86_64/bin/protoc # Available at <https://github.com/google/protobuf>.
PROTOC_GEN_DOC=/usr/bin/protoc-gen-doc           # Available at <https://github.com/estan/protoc-gen-doc>.

today=$(shell date +%e\ %B\ %Y)
sections:=$(wildcard [0-9][0-9]-*.md)


esda-manual.docx: $(sections)
	pandoc --standalone --smart --metadata date="$(today)" --output=esda-manual.docx $^

esda-manual.html: $(sections)
	pandoc --standalone --smart --metadata date="$(today)" --output=esda-manual.html $^

clean:
	-rm esda-manual.{docx,html}

02-api.md: esda_records_4.proto templates/records-api.mustache
	$(PROTOC) --plugin=$(PROTOC_GEN_DOC) --doc_out=templates/records-api.mustache,$@:./ $<

90-protobuf.md: esda_records_4.proto

timestamp:
	sed -r -e 's/^% ..? .+ ....$$/% $(today)/' 00-front.md > z && mv z 00-front.md


.PRECIOUS:

.SUFFIXES:
