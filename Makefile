SHELL=bash

# This makefile is known to work with the following versions of its toolchain:
#   protoc          3.0.0    <https://github.com/google/protobuf>
#   protoc-gen-doc  0.9      <https://github.com/estan/protoc-gen-doc>
#   pandoc          1.19.2.1
#   pandoc-citeproc 0.10.4.1
#   mermaid         7.0.0    <http://knsv.github.io/mermaid/>
#   phantomjs       2.1.1    <http://phantomjs.org/download.html>

PROTOC=/opt/protoc-3.0.0-linux-x86_64/bin/protoc
PROTOC_GEN_DOC=/usr/bin/protoc-gen-doc
MERMAID=/usr/local/bin/mermaid
PHANTOM=/opt/phantomjs-2.1.1-linux-x86_64/bin/phantomjs


today=$(shell date +%e\ %B\ %Y)
sections:=$(shell ls -1 [0-9][0-9]-*.md | sort)
diagrams:=$(shell ls -1 *.mermaid | sed -e 's/$$/.png/')


all: esda-manual.pdf esda-manual.docx esda-manual.html

clean:
	-rm esda-manual.{pdf,docx,html} esda-slides.html $(diagrams)

veryclean: clean
	touch --date="1970-01-01" 04-api.md 11-protobuf.md


esda-manual.%: $(sections) $(diagrams) references.bib
	pandoc --standalone                  \
	       --smart                       \
	       --columns 1000                \
	       --number-sections             \
	       --table-of-contents           \
	       --toc-depth=2                 \
	       --css esda.css                \
	       --metadata date="$(today)"    \
	       --bibliography=references.bib \
	       --filter pandoc-citeproc      \
	       --csl chicago-author-date.csl \
	       --mathjax                     \
	       --output=$@ $(sections)

esda-slides.html: $(sections) $(diagrams) references.bib
	pandoc --self-contained                 \
	       --smart                          \
	       --metadata date="$(today)"       \
	       --bibliography=references.bib    \
	       --filter pandoc-citeproc         \
	       --csl chicago-author-date.csl    \
	       --to slidy                       \
	       --slide-level=2                  \
	       --output $@ $(sections) 

04-api.md: esda_records_4.proto templates/records-api.mustache
	$(PROTOC) --plugin=$(PROTOC_GEN_DOC) --doc_out=templates/records-api.mustache,$@:./ $<

11-protobuf.md: esda_records_4.proto
	sed -e '1i# Appendices\n## Protocol Buffers for Records API Version 4\n' -e '/^\//d ; /^ \*/d ; s/\/\/\/ [^[].*// ; s/\(\/\/\/ \[[^]]*\]\).*/\1/ ; s/^/\t/' $< | uniq > $@

timestamp:
	sed -i -r -e 's/^% ..? .+ ....$$/% $(today)/' 00-front.md


%.mermaid.png: %.mermaid
	$(MERMAID) --phantomPath $(PHANTOM) $< | grep -E -v '(DEBUG|INFO)'


.PRECIOUS: $(diagrams)

.SUFFIXES:
