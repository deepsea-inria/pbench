.PHONY: all
all:
	@ dune build

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: install
install:
	@ dune build -p pbench
	@ dune install -p pbench

.PHONY: uninstall
uninstall:
	@ dune build -p pbench
	@ dune uninstall -p pbench

# Usage: [make release VERSION=X.Y]

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make uninstall
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
# Check if this is the master branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Make sure $(VERSION) is nonempty.
	@ if [ -z "$(VERSION)" ] ; then \
	    echo "Error: please use: make release VERSION=X.Y.Z" ; \
	    exit 1 ; \
	  fi
# Make sure a CHANGES entry with the current version seems to exist.
	@ if ! grep "## v$(VERSION)" Changes.md >/dev/null ; then \
	    echo "Error: Changes.md has no entry for version $(VERSION)." ; \
	    exit 1 ; \
	  fi
# Make sure the current version is mentioned in dune-project.
	@ if ! grep "(version $(VERSION))" dune-project >/dev/null ; then \
	    echo "Error: dune-project does not mention version $(VERSION)." ; \
	    grep "(version" dune-project ; \
	    exit 1 ; \
	  fi
# Create a git tag.
	@ git tag v$(VERSION)
# Dump $(VERSION) into a file.
	@ echo "$(VERSION)" > .version
# Done.
	@ echo "Created (local) git tag $(VERSION)."
	@ echo "To push this tag and publish on opam, please type:"
	@ echo "  \"make publish\""
	@ echo "To delete this tag, please type:"
	@ echo "  \"make undo\""

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish --tag=v$(shell cat .version) -v $(shell cat .version)

.PHONY: undo
undo:
# Delete the tag that was created above.
	@ git tag -d v$(shell cat .version)
