#!/bin/sh
set -eu

elm-go src/Main.elm --hot --dir docs --open -- --output="docs/elm.js" --debug
