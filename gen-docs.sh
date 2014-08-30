#!/bin/bash

cabal haddock --hoogle --hyperlink-source --html-location='http://hackage.haskell.org/package/$pkg/docs' --contents-location='http://hackage.haskell.org/package/$pkg'
