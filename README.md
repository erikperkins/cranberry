# Cranberry
This is the Haskell component of Data Punnet. It is a JSON API built on the high-performance [Snap framework](www.snapframework.com).

## Build
The `cabal` build tool must be used with some measure of discipline in order to avoid dependency conflicts. A simple and effective approach is to build in a sandbox
```
$ cabal sandbox init
$ cabal install
```
If a dependency is added after the build, it may be necessary to rebuild the sandbox
```
$ cabal sandbox delete
$ cabal sandbox init
$ cabal install
```
