PROJECT_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

.PHONY: fmt fmt-haskell fmt-nix

fmt: fmt-hs fmt-nix

fmt-hs:
	ormolu --mode inplace $$(find ${PROJECT_DIR} -name '*.hs' -not -path "*/dist-newstyle/*")

fmt-nix:
	nixfmt $$(find ${PROJECT_DIR} -name '*.nix')
