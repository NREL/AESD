SHELL=bash

PROTOC=/opt/protoc-3.0.0-linux-x86_64/bin/protoc        # Available at <https://github.com/google/protobuf>.
PROTOC_GEN_DOC=/usr/bin/protoc-gen-doc                  # Available at <https://github.com/estan/protoc-gen-doc>.
MERMAID=/usr/local/bin/mermaid                          # Available at <http://knsv.github.io/mermaid/>.
PHANTOM=/opt/phantomjs-2.1.1-linux-x86_64/bin/phantomjs # Available at <http://phantomjs.org/download.html>.


today=$(shell date +%e\ %B\ %Y)
sections:=$(wildcard [0-9][0-9]-*.md)
diagrams:=$(wildcard *.mermaid.png)


all: esda-manual.docx esda-manual.html


esda-manual.docx: $(sections) $(diagrams)
	pandoc --standalone --smart --metadata date="$(today)" --output=esda-manual.docx $(sections)

esda-manual.html: $(sections)
	pandoc --standalone --smart --metadata date="$(today)" --output=esda-manual.html $^

clean:
	-rm esda-manual.{docx,html}

02-api.md: esda_records_4.proto templates/records-api.mustache
	$(PROTOC) --plugin=$(PROTOC_GEN_DOC) --doc_out=templates/records-api.mustache,$@:./ $<

90-protobuf.md: esda_records_4.proto
	sed -e '1i# Appendix: ESDA Records Version 4 Protocol Buffers\n' -e '/^\//d ; /^ \*/d ; s/\/\/\/ [^[].*// ; s/\(\/\/\/ \[[^]]*\]\).*/\1/ ; s/^/\t/' $< | uniq > $@

timestamp:
	sed -i -r -e 's/^% ..? .+ ....$$/% $(today)/' 00-front.md


%.mermaid.png: %.mermaid
	$(MERMAID) --phantomPath $(PHANTOM) $<


.PRECIOUS:

.SUFFIXES:
