# Minimal makefile for Sphinx documentation
# You can set these variables from the command line, and also
# from the environment .

LANGUAGES     ?= en de zh_CN es fr nl ja
SPHINXINTL    ?= sphinx-intl
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
LOCALEDIR     ?= locale

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

html: $(addprefix html/,$(LANGUAGES)) $(BUILDDIR)/html/en/index.html
	@echo "Pages available at file://$$PWD/$(BUILDDIR)/html/en/index.html"

$(addprefix html/,$(LANGUAGES)): $(MAKEFILES)
	@$(SPHINXBUILD) "$(SOURCEDIR)" "$(BUILDDIR)/$@" $(SPHINXOPTS) -Dlanguage=$(word 2,$(subst /, ,$@))

$(BUILDDIR)/html/index.html: html/index.html
	@cp $< $@

gettext: $(MAKEFILES)
	@$(SPHINXBUILD) -b $@ "$(SOURCEDIR)" "$(BUILDDIR)/$@" $(SPHINXOPTS)
	@$(SPHINXINTL) update -p "$(BUILDDIR)/$@" -d locale $(addprefix -l,$(filter-out en,$(LANGUAGES)))