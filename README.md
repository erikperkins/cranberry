# Cranberry [![Build Status](https://travis-ci.com/erikperkins/cranberry.svg?branch=master)](https://travis-ci.com/erikperkins/cranberry) [![Coverage Status](https://coveralls.io/repos/github/erikperkins/cranberry/badge.svg?branch=master)](https://coveralls.io/github/erikperkins/cranberry?branch=master)
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

## Test
```
$ cabal test
```
