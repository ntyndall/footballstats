Currently a manual run has to be performed by sourcing the package and running

```main()```

[![Build Status](https://travis-ci.org/niallbenj/footballstats.svg?branch=master)](https://travis-ci.org/niallbenj/footballstats)
[![codecov](https://codecov.io/gh/niallbenj/footballstats/branch/master/graph/badge.svg)](https://codecov.io/gh/niallbenj/footballstats)

## Building ineractively
Inside `/R/` there is a file called `local.R`. Source the functionality interactively, then run, `reload(dir)`, where `dir` is your current working directory.

## Tagging a release

Run `Rscript tag.R -t {version #}` inside the root directory to tag a new release and save the zip file inside a folder `/tagged/`.
