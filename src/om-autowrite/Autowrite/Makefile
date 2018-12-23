# Makefile pour projet Lisp

.SUFFIXES: .lisp

all:

	.PHONY clean clean-objs

clean:
	-rm -f *~

clean-objs:
	find . -name \*.fasl -print -exec rm \{\} \;

clean-visu:
	rm -f {visu-aw,interface-variables,time,process,toy,presentations,completion,processes,utils-interface,interface}.{x86f,err,fasl}
