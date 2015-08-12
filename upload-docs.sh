#!/bin/bash
set -e

user=$1

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=bloodhound
ver=0.7.0.1

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

echo "Detected package: $pkg-$ver"

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs

tar cvz -C $dir --format=ustar -f $dir/$pkg-$ver-docs.tar.gz $pkg-$ver-docs

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u "$user" \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/docs"
