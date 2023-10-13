# ****************************************
# Variable Selection Simulation Study
# 
# Linear Regression Results Examination
# 
# Emma Tarmey
#
# Started:          19/06/2023
# Most Recent Edit: 13/10/2023
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

coef.tables <- function(method = "linear", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4) {
  message( paste("\n\n", method, " Parameter Estimates for each Scenario") )
  
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
  coef.results.s1 <- (coef.results.s1 %>% dplyr::select("Technique", "Variable", "Value"))
  coef.results.s2 <- (coef.results.s2 %>% dplyr::select("Technique", "Variable", "Value"))
  coef.results.s3 <- (coef.results.s3 %>% dplyr::select("Technique", "Variable", "Value"))
  coef.results.s4 <- (coef.results.s4 %>% dplyr::select("Technique", "Variable", "Value"))
  
  # restrict to only models of a given method
  method.coef.results.s1 <- (coef.results.s1 %>% filter(Technique == method))
  method.coef.results.s2 <- (coef.results.s2 %>% filter(Technique == method))
  method.coef.results.s3 <- (coef.results.s3 %>% filter(Technique == method))
  method.coef.results.s4 <- (coef.results.s4 %>% filter(Technique == method))
  
  # find the mean value of every coefficient, per variable and per scenario
  method.coef.means.s1 <- aggregate(Value ~ Variable, data = method.coef.results.s1, mean)
  method.coef.means.s2 <- aggregate(Value ~ Variable, data = method.coef.results.s2, mean)
  method.coef.means.s3 <- aggregate(Value ~ Variable, data = method.coef.results.s3, mean)
  method.coef.means.s4 <- aggregate(Value ~ Variable, data = method.coef.results.s4, mean)
  
  colnames(method.coef.means.s1) <- c("Variable", "Mean")
  colnames(method.coef.means.s2) <- c("Variable", "Mean")
  colnames(method.coef.means.s3) <- c("Variable", "Mean")
  colnames(method.coef.means.s4) <- c("Variable", "Mean")
  
  # find the standard deviation of every coefficient, per variable and per scenario
  method.coef.sds.s1 <- aggregate(Value ~ Variable, data = method.coef.results.s1, sd)
  method.coef.sds.s2 <- aggregate(Value ~ Variable, data = method.coef.results.s2, sd)
  method.coef.sds.s3 <- aggregate(Value ~ Variable, data = method.coef.results.s3, sd)
  method.coef.sds.s4 <- aggregate(Value ~ Variable, data = method.coef.results.s4, sd)
  
  colnames(method.coef.sds.s1) <- c("Variable", "SD")
  colnames(method.coef.sds.s2) <- c("Variable", "SD")
  colnames(method.coef.sds.s3) <- c("Variable", "SD")
  colnames(method.coef.sds.s4) <- c("Variable", "SD")
  
  # aggregate the above to form our summary
  method.coef.summary.s1 <- merge(method.coef.means.s1, method.coef.sds.s1, by = "Variable")
  method.coef.summary.s2 <- merge(method.coef.means.s2, method.coef.sds.s2, by = "Variable")
  method.coef.summary.s3 <- merge(method.coef.means.s3, method.coef.sds.s3, by = "Variable")
  method.coef.summary.s4 <- merge(method.coef.means.s4, method.coef.sds.s4, by = "Variable")
  
  # re-order rows to match canonical ordering
  order <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
  method.coef.summary.s1 <- ( method.coef.summary.s1 %>% slice(match(order, Variable)) )
  method.coef.summary.s2 <- ( method.coef.summary.s2 %>% slice(match(order, Variable)) )
  method.coef.summary.s3 <- ( method.coef.summary.s3 %>% slice(match(order, Variable)) )
  method.coef.summary.s4 <- ( method.coef.summary.s4 %>% slice(match(order, Variable)) )
  
  # contextualize by pulling true parameter values from data generation
  method.coef.summary.s1$True <- get.true.param.1()
  method.coef.summary.s2$True <- get.true.param.2()
  method.coef.summary.s3$True <- get.true.param.3()
  method.coef.summary.s4$True <- get.true.param.4()
  
  # re-order columns
  method.coef.summary.s1 <- (method.coef.summary.s1 %>% dplyr::select("Variable", "True", "Mean", "SD"))
  method.coef.summary.s2 <- (method.coef.summary.s2 %>% dplyr::select("Variable", "True", "Mean", "SD"))
  method.coef.summary.s3 <- (method.coef.summary.s3 %>% dplyr::select("Variable", "True", "Mean", "SD"))
  method.coef.summary.s4 <- (method.coef.summary.s4 %>% dplyr::select("Variable", "True", "Mean", "SD"))
  
  # save results
  method.coef.summary.s1 %>% write.csv(., paste("../data/coef_summary_", method, "_s1.csv"))
  method.coef.summary.s2 %>% write.csv(., paste("../data/coef_summary_", method, "_s2.csv"))
  method.coef.summary.s3 %>% write.csv(., paste("../data/coef_summary_", method, "_s3.csv"))
  method.coef.summary.s4 %>% write.csv(., paste("../data/coef_summary_", method, "_s4.csv"))
  
  return( list(method.coef.summary.s1, method.coef.summary.s2,  method.coef.summary.s3, method.coef.summary.s4 ) )
}

