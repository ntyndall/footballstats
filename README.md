Currently a manual run has to be performed by sourcing the package and running

```main()```

[![Build Status](https://travis-ci.org/niallbenj/footballstats.svg?branch=master)](https://travis-ci.org/niallbenj/footballstats)
[![codecov](https://codecov.io/gh/niallbenj/footballstats/branch/master/graph/badge.svg)](https://codecov.io/gh/niallbenj/footballstats)

## Building ineractively
Inside `/R/` there is a file called `local.R`. Source the functionality interactively, then run, `reload(dir)`, where `dir` is your current working directory.

## Tagging a release

Run `Rscript tag.R -t` inside the root directory to tag a new release and save the zip file inside a folder `/tagged/`. The script will check the current version saved in GIT and will only create a new tar if the version in the DESCRIPTION file is > GIT TAG. Once this is done then update github with the newest release. So some manual steps are required. (The script should warn when something doesn't match up in terms of versioning and reports on a successful tagging)
