.PHONY: all
all:
	cabal run

haddock:
	cabal v2-haddock app/RecordM.hs
