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


all: ../docs/aesd-manual.pdf ../docs/aesd-manual.docx ../docs/aesd-manual.html

clean:
	-rm ../docs/aesd-manual.{pdf,docx,html} ../docs/aesd-slides.html $(diagrams)

veryclean: clean
	touch --date="1970-01-01" 04-api.md 11-protobuf.md


../docs/aesd-manual.%: $(sections) $(diagrams) references.bib
	pandoc --standalone                  \
	       --smart                       \
	       --columns 1000                \
	       --number-sections             \
	       --table-of-contents           \
	       --toc-depth=2                 \
	       --css aesd.css                \
	       --metadata date="$(today)"    \
	       --bibliography=references.bib \
	       --filter pandoc-citeproc      \
	       --csl chicago-author-date.csl \
	       --self-contained              \
	       --output=$@ $(sections)

../docs/aesd-slides.html: $(sections) $(diagrams) references.bib
	pandoc --self-contained                 \
	       --smart                          \
	       --metadata date="$(today)"       \
	       --bibliography=references.bib    \
	       --filter pandoc-citeproc         \
	       --csl chicago-author-date.csl    \
	       --to slidy                       \
	       --slide-level=2                  \
	       --output $@ $(sections) 

04-api.md: aesd_records_4.proto templates/records-api.mustache
	$(PROTOC) --plugin=$(PROTOC_GEN_DOC) --doc_out=templates/records-api.mustache,$@:./ $<

11-protobuf.md: aesd_records_4.proto
	sed -e '1i# Appendices\n## Protocol Buffers for Records API Version 4\n' -e '/^\//d ; /^ \*/d ; s/\/\/\/ [^[].*// ; s/\(\/\/\/ \[[^]]*\]\).*/\1/ ; s/^/\t/' $< | uniq > $@

timestamp:
	sed -i -r -e 's/^% ..? .+ ....$$/% $(today)/' 00-front.md


%.mermaid.png: %.mermaid
	$(MERMAID) --phantomPath $(PHANTOM) $< | grep -E -v '(DEBUG|INFO)'


.PRECIOUS: $(diagrams)

.SUFFIXES:
