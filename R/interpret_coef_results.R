# ****************************************
# Variable Selection Simulation Study
# 
# Linear Regression Results Examination
# 
# Emma Tarmey
#
# Started:          19/06/2023
# Most Recent Edit: 03/10/2023
# ****************************************



# ----- Preamble -----

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggcorrplot)
  library(ggdag)
  library(ggplot2)
  library(glmnet)
  library(ncvreg)
  library(scales)
  library(stringr)
  library(tidyverse)
})

if ( !is.element( "RStudio", commandArgs() ) ) {
  setwd("../R") # only needed for bash version, not running in RStudio
}

source("generate_data.R")
source("plot_rescale.R")

# ----- Read Results Data from CSV -----

get.results.data <- function() {
  load("../data/bias_results_s1.rda")
  load("../data/bias_results_s2.rda")
  load("../data/bias_results_s3.rda")
  load("../data/bias_results_s4.rda")
  
  load("../data/coef_results_s1.rda")
  load("../data/coef_results_s2.rda")
  load("../data/coef_results_s3.rda")
  load("../data/coef_results_s4.rda")
  
  return ( list( bias.results.s1, bias.results.s2, bias.results.s3, bias.results.s4,
                 coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4 ))
}




# ----- Linear Regression Error Investigation -----

coef.tables <- function(coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4) {
  message("\n\nLinear Regression Parameter Estimates for each Scenario")
  
  # convert results matrices to standardised data-frame
  coef.results.s1 <- ( coef.results.s1 %>% reshape2::melt() )
  coef.results.s2 <- ( coef.results.s2 %>% reshape2::melt() )
  coef.results.s3 <- ( coef.results.s3 %>% reshape2::melt() )
  coef.results.s4 <- ( coef.results.s4 %>% reshape2::melt() )
  
  # label columns
  colnames(coef.results.s1) <- c("Iteration", "Technique", "Variable", "Value")
  colnames(coef.results.s2) <- c("Iteration", "Technique", "Variable", "Value")
  colnames(coef.results.s3) <- c("Iteration", "Technique", "Variable", "Value")
  colnames(coef.results.s4) <- c("Iteration", "Technique", "Variable", "Value")
  
  # ignore iteration column
  coef.results.s1 <- (coef.results.s1 %>% select("Technique", "Variable", "Value"))
  coef.results.s2 <- (coef.results.s2 %>% select("Technique", "Variable", "Value"))
  coef.results.s3 <- (coef.results.s3 %>% select("Technique", "Variable", "Value"))
  coef.results.s4 <- (coef.results.s4 %>% select("Technique", "Variable", "Value"))
  
  # restrict to only linear regression models
  lr.coef.results.s1 <- (coef.results.s1 %>% filter(Technique == "linear"))
  lr.coef.results.s2 <- (coef.results.s2 %>% filter(Technique == "linear"))
  lr.coef.results.s3 <- (coef.results.s3 %>% filter(Technique == "linear"))
  lr.coef.results.s4 <- (coef.results.s4 %>% filter(Technique == "linear"))
  
  # find the mean value of every coefficient, per variable and per scenario
  lr.coef.means.s1 <- aggregate(Value ~ Variable, data = lr.coef.results.s1, mean)
  lr.coef.means.s2 <- aggregate(Value ~ Variable, data = lr.coef.results.s2, mean)
  lr.coef.means.s3 <- aggregate(Value ~ Variable, data = lr.coef.results.s3, mean)
  lr.coef.means.s4 <- aggregate(Value ~ Variable, data = lr.coef.results.s4, mean)
  
  colnames(lr.coef.means.s1) <- c("Variable", "Mean")
  colnames(lr.coef.means.s2) <- c("Variable", "Mean")
  colnames(lr.coef.means.s3) <- c("Variable", "Mean")
  colnames(lr.coef.means.s4) <- c("Variable", "Mean")
  
  # find the standard deviation of every coefficient, per variable and per scenario
  lr.coef.sds.s1 <- aggregate(Value ~ Variable, data = lr.coef.results.s1, sd)
  lr.coef.sds.s2 <- aggregate(Value ~ Variable, data = lr.coef.results.s2, sd)
  lr.coef.sds.s3 <- aggregate(Value ~ Variable, data = lr.coef.results.s3, sd)
  lr.coef.sds.s4 <- aggregate(Value ~ Variable, data = lr.coef.results.s4, sd)
  
  colnames(lr.coef.sds.s1) <- c("Variable", "SD")
  colnames(lr.coef.sds.s2) <- c("Variable", "SD")
  colnames(lr.coef.sds.s3) <- c("Variable", "SD")
  colnames(lr.coef.sds.s4) <- c("Variable", "SD")
  
  # aggregate the above to form our summary
  lr.coef.summary.s1 <- merge(lr.coef.means.s1, lr.coef.sds.s1, by = "Variable")
  lr.coef.summary.s2 <- merge(lr.coef.means.s2, lr.coef.sds.s2, by = "Variable")
  lr.coef.summary.s3 <- merge(lr.coef.means.s3, lr.coef.sds.s3, by = "Variable")
  lr.coef.summary.s4 <- merge(lr.coef.means.s4, lr.coef.sds.s4, by = "Variable")
  
  # re-order rows to match canonical ordering
  order <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
  lr.coef.summary.s1 <- ( lr.coef.summary.s1 %>% slice(match(order, Variable)) )
  lr.coef.summary.s2 <- ( lr.coef.summary.s2 %>% slice(match(order, Variable)) )
  lr.coef.summary.s3 <- ( lr.coef.summary.s3 %>% slice(match(order, Variable)) )
  lr.coef.summary.s4 <- ( lr.coef.summary.s4 %>% slice(match(order, Variable)) )
  
  # contextualize by pulling true parameter values from data generation
  lr.coef.summary.s1$True <- get.true.param.1()
  lr.coef.summary.s2$True <- get.true.param.2()
  lr.coef.summary.s3$True <- get.true.param.3()
  lr.coef.summary.s4$True <- get.true.param.4()
  
  # re-order columns
  lr.coef.summary.s1 <- (lr.coef.summary.s1 %>% select("Variable", "True", "Mean", "SD"))
  lr.coef.summary.s2 <- (lr.coef.summary.s2 %>% select("Variable", "True", "Mean", "SD"))
  lr.coef.summary.s3 <- (lr.coef.summary.s3 %>% select("Variable", "True", "Mean", "SD"))
  lr.coef.summary.s4 <- (lr.coef.summary.s4 %>% select("Variable", "True", "Mean", "SD"))
  
  return( list(lr.coef.summary.s1, lr.coef.summary.s2,  lr.coef.summary.s3, lr.coef.summary.s4 ) )
}

