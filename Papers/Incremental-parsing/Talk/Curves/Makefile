FILES=$(wildcard *.res)
TARGETS=$(FILES:.res=.eps)
SCRIPTS=$(FILES:.res=.gp)
PDF=$(FILES:.res=-eps-converted-to.pdf)

all: $(TARGETFILES)
	echo $(PDF)
	for i in $(SCRIPTS) ; do \
	gnuplot ./$$i ; \
	done

clean:
	-rm -f *~

spotless:
	-rm -f $(TARGETS)
	-rm -f $(PDF)
