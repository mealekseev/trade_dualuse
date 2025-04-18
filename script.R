### The script executes code files for the paper

setwd("/Users/malekseev/trade_dualuse")

## Part 0. Generate auxiliary crosswalks
source("crosswalks/00_build_crosswalks.R")

## Part I. Motivating facts
source("motivation/01_build_dualuse.R")
source("motivation/02_run_gravity.R")

## Part II. Empirical measurement
source("measurement/03_build_measure.R")
source("measurement/04_validate_measure.R")
source("measurement/05_evaluate_measure.R")

## Part III. Calibration
source("calibration/06_fit_scale.R")
source("calibration/07_build_networks.R")
source("calibration/08_run_model.R")

