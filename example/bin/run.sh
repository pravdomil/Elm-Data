#!/usr/bin/env bash

# Stop if any command fails.
set -e

# Stop on unset variables.
set -u

# Be in project root.
cd "${0%/*}/.."

# Have dependencies from npm ready.
npm i

# Compile application.
elm make src/Main.elm --output "dist/elm.js"
elm-ffi --run "dist/elm.js"

# Run application.
cd dist
node elm.js
