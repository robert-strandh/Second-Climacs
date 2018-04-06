FILES=$(wildcard *.res)
TARGETS=$(FILES:.res=.eps)
SCRIPTS=$(FILES:.res=)

all: $(TARGETFILES)
	for i in $(SCRIPTS) ; do \
	echo $$i ; \
	$$i ; \
	done

clean:
	-rm -f *~

spotless:
	-rm -f $(TARGETS)
