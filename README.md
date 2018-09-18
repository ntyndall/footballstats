<img align="right" width="125" height="150" src="https://raw.githubusercontent.com/ntyndall/footballstats/master/images/fs_sticker.png">

## Code status
[![Build Status](https://travis-ci.org/ntyndall/footballstats.svg?branch=master)](https://travis-ci.org/ntyndall/footballstats)
[![codecov](https://codecov.io/gh/ntyndall/footballstats/branch/master/graph/badge.svg)](https://codecov.io/gh/ntyndall/footballstats)


## Installing and running this package

```r
# Run this as a script

# Install football stats from github
cat(' ## Installing footballstats \n ## ')

library(devtools)

devtools::install_github(
  repo = "ntyndall/footballstats"
)

library(footballstats)

cat(' ## Install Complete. \n\n')

# Set up log directory
footballstats::create_log_dir()
``` 

#### Deployment
  - Download and install [redis](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-redis) then from the command line start it by typing
```shell
cd redis-stable/utils
sudo ./install_server.sh
```
To start and stop the redis server type `sudo service redis_6379 start / stop`
  - Edit the `/usr/lib/R/etc/Rprofile.site` file and include the following 3 tokens
```r
Sys.setenv(FS_HOST = "___")
Sys.setenv(FS_APIKEY = "Authorization=___")
Sys.setenv(FS_SLACK = "___")
Sys.setenv(FS_DEPLOYLOC = "___")
```
where the last token is the root path of where the above script is run from, e.g. `/root/`.
  - Set up a cron job by typing `crontab -e` with the following...
```shell
# Collect football information - such as match / team / commentary information THEN predict matches
0 22 * * 0 Rscript -e 'library(footballstats); footballstats::analyse_and_predict(deployed = TRUE)'

# After information has been gathered - update player status etc (COSTLY operation!)
0 22 * * 1 Rscript -e 'library(footballstats); footballstats::analyse_players(deployed = TRUE)'

# Generate report
# ...
```

Unfortunately, the size of the XGBoost model being stored in the R package leads to memory issues when lazy loading - to save some cash on large memory boxes, I _can_ just `scp mymodels/xgModel.rda root@IP:/root` the file across and load it while performing the analysis so I don't have to lazy load it.

## Tagging a release
  - Update `DESCRIPTION` file to required version (must be > than most recent `GIT` version).
  - Run `Rscript -e "organisR::tag()"` inside root directory `/footballstats/`.
  - Update Github with the newest tag and binary that is produced from step 2.

## Scripts
There are a number of useful scripts for testing and building models. Make sure that `redis` is running on port `6379`.

  - In order to run the tests on a local machine, run from the root directory;
    - `Rscript demo/run_tests.R`
  - To rebuild one of the data models with updated information in redis, run from the root directory
    - `Rscript demo/create_nn.R`

## Accuracy
Below will be information on the accuracy so far that the app is achieving. To add more results, run through the data sets and append to the `data/accuracyFrame` data set in a similar fashion and from the root directory create the plot by running `Rscript demo/create_acc_plot.R` and push the new image to the repo.
