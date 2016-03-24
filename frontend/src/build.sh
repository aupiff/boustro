#!/bin/bash

stack build
echo "Copy all.js..."
cp ../.stack-work/install/x86_64-osx/lts-3.14/ghcjs-0.2.0.20151029_ghc-7.10.2/bin/ghcjs-jquery-example.jsexe/all.js .
echo "All done."
