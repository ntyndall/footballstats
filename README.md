## Code status
[![Build Status](https://travis-ci.org/niallbenj/footballstats.svg?branch=master)](https://travis-ci.org/niallbenj/footballstats)
[![codecov](https://codecov.io/gh/niallbenj/footballstats/branch/master/graph/badge.svg)](https://codecov.io/gh/niallbenj/footballstats)

## Installing and running this package
```
library(devtools)
install_github("niallbenj/footballstats")
library(footballstats)

# To run the script run
footballstats::main()
``` 

#### Deployment
  - Download and install [redis](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-redis) then from the command line start it by typing
```
cd utils
sudo ./install_server.sh
```
To start and stop the redis server type `sudo service redis_6379 start / stop`
  - Edit the `/usr/lib/R/etc/Rprofile.site` file and include the following 3 tokens
```
Sys.setenv(FS_HOST = "___")
Sys.setenv(FS_APIKEY = "Authorization=___")
Sys.setenv(FS_SLACK = "___")
```
  - Set up a cron job by typing `crontab -e` with the following...
```
0 22 * * 0 Rscript /root/collect_and_predict.R
0 22 * * 1 Rscript /root/collect_players.R
0 22 1 * * Rscript /root/generate_report.R
```
and create the following files in `/root/` : 

```
# /root/collect_and_predict.R
library(footballstats)

footballstats::main()
```

```
# /root/collect_players.R
library(footballstats)

footballstats::analyse_players()
```

```
# /root/generate_report.R
library(footballstats)

footballstats::monthly_report()
```

## Tagging a release
  - Update `DESCRIPTION` file to required version (must be > than most recent `GIT` version).
  - Run `Rscript tag.R {v#}` inside root directory `/footballstats/`.
  - Update Github with the newest tag and binary that is produced from step 2.
