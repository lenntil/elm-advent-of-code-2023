#!/bin/sh
set -eu

if command -v elm-go >/dev/null 2>&1; then
	elm-go src/Main.elm --hot --dir docs --open -- --output="docs/elm.js" --debug
else
	elm-live src/Main.elm --hot --dir docs --open -- --output="docs/elm.js" --debug
fi
