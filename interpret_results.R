# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 24/04/2023
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

load("data/results_s1.rda")
load("data/results_s2.rda")


# ----- Examine Results -----

results.s1 %>% dim %>% print
results.s2 %>% dim %>% print

results.s1[1, , ] %>% knitr::kable()
results.s2[1, , ] %>% knitr::kable()


# ----- Interpreting Results -----

message("Mean Bias of each VS Technique for each Parameter estimate")

message(paste(c("Scenario = ", 1, ", N = ", dim(results.s1)[1] ), sep = ""))
results.s1 %>% apply(., c(2, 3), mean) %>% knitr::kable()
message(paste(c("Scenario = ", 2, ", N = ", dim(results.s1)[1]), sep = ""))
results.s2 %>% apply(., c(2, 3), mean) %>% knitr::kable()


# ----- Generate Plots

# Heat Map
png("plots/bias_s1.png")
p <- (results.s1 %>% apply(., c(2, 3), mean)) %>% ggcorrplot::ggcorrplot() +
  ggtitle("Bias HeatMap - Scenario 1")
p
dev.off()


png("plots/bias_s2.png")
p <- (results.s2 %>% apply(., c(2, 3), mean)) %>% ggcorrplot::ggcorrplot() +
  ggtitle("Bias HeatMap - Scenario 2")
p
dev.off()

