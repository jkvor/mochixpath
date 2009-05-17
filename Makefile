VERSION=0.1
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

clean:
	rm -f ebin/*.beam erl_crash.dump

package: clean
	@mkdir mochixpath-$(VERSION)/ && cp -rf html-docs Makefile Readme src t mochixpath-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf mochixpath-$(VERSION).tgz mochixpath-$(VERSION)
	@rm -rf mochixpath-$(VERSION)/

install: all
	mkdir -p $(prefix)/$(LIBDIR)/mochixpath-$(VERSION)/ebin
	for i in ebin/*.beam; do install $$i $(prefix)/$(LIBDIR)/mochixpath-$(VERSION)/$$i ; done
	
test: all
	prove -v t/*.t

./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<