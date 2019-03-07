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

## Intellij IDEA
Integration with Intellij IDEA can be done with the HaskForce plugin. This is a
barebones plugin which allows Intellij to build `cabal` projects. The plugin has
a few issues which require workarounds relative to the usual approach in
Intellij.

To register an existing `cabal` project with Intellij, select
`Tools > Discover Cabal Packages`; this should find `cranberry.cabal`,
and allow the `Build` button to compile the project using `cabal`. If this is
not done, the `Build` button will still appear to function, but nothing will be
compiled.

Intellij will generate a `cranberry.iml` file and an `.idea/` directory. These
should  be added to `.gitignore` and `.dockerignore`. To prevent them from
showing in the `Project` sidebar, in the `Settings` dialog, find
`Editor > File Types > Ignore files and folders`, and add `*.iml;.idea/;` to the
list of ignored objects.

HaskForce has a minimal run configuration for `cabal`, but it does not allow the
specification of environment variables. To work around this limitation, use a
`bash` run configuration instead. In the `Edit Configurations` dialog, add a
bash script run configuration, and enter the following:
```
Script: /usr/bin/cabal run
Interpreter path: /bin/bash
Interpreter options: -c
```
This will run the command `/bin/bash -c "/usr/bin/cabal run"`, using the
environment available to `root`. Specify the necessary environment variables in
the run configuration (e.g. `REDIS_HOST`). This may also be necessary for test
run configurations, depending on how environment variables are specified in the
tests.
