# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 17/04/2023
# ****************************************


# ----- Preamble -----


suppressPackageStartupMessages({
  library(broom)
  library(data.table)
  library(dplyr)
  library(ggdag)
  library(ggplot2)
  library(glmnet)
  library(gridExtra)
  library(knitr)
  library(modelr)
  library(ncvreg)
  library(OpenMx)
  library(scales)
  library(sgee)
  library(stringr)
  library(tidyverse)
  library(VARSELECTEXPOSURE)
})

rm(list = ls())


# ----- Read Results Data from CSV -----

results.s1 <- read.csv("data/results_s1.csv")
results.s2 <- read.csv("data/results_s2.csv")


# ----- Look at Results -----

results.s1 %>% head %>% knitr::kable()
results.s2 %>% head %>% knitr::kable()


# ----- Interpreting Results -----

results.s1 %>% colMeans() %>% knitr::kable(col.names = c("Mean Bias"))
results.s2 %>% colMeans() %>% knitr::kable(col.names = c("Mean Bias"))



