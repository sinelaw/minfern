# Top-level Makefile for the FormalMinfern Coq development.
#
# `make`       — build everything (delegates to coq_makefile-generated rules).
# `make clean` — remove generated files.
# `make Makefile.coq` — regenerate the inner Makefile from `_CoqProject`.

.PHONY: all clean cleanall

all: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: _CoqProject
	coq_makefile -f _CoqProject -o Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq Makefile.coq.conf

cleanall: clean
	find theories -name '*.vo' -o -name '*.vok' -o -name '*.vos' -o -name '*.glob' -o -name '*.aux' \
	    | xargs -r rm -f
