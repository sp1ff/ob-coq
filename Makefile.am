dist_lisp_LISP = inf-coq.el ob-coq.el ob-coq-pkg.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp ob-coq-$(PACKAGE_VERSION).tar
SUBDIRS = test
AM_ELCFLAGS = -L contrib --eval '(require (quote bytecomp))'

package: ob-coq-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

ob-coq-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	mkdir -p .pkg-tmp/ob-coq-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/ob-coq-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/ob-coq-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ ob-coq-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/ob-coq-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package

