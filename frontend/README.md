boustro frontend
================

```
~/reflex-platform/work-on ghcjs ./. --run "ghcid -c'cabal repl'"
```

using current submodule scheme:

```
./reflex-platform/work-on ./ghcjs-packages.nix ./.
```

if things get updated, you may need to run:
```
cabal configure --ghcjs
```
