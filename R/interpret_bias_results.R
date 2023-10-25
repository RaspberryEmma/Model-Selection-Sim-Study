# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# This file generates all relevant summary tables
# 
# Emma Tarmey
#
# Started:          14/04/2023
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



# ----- Generate Summary Statistics, Produce Mean Bias Results -----

bias.tables <- function(bias.results.s1, bias.results.s2, bias.results.s3, bias.results.s4,
                        coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4) {
  message("\n\nRaw Bias Values:\n")
  
  # examine raw numbers
  bias.results.s1[1, , ] %>% knitr::kable()
  bias.results.s2[1, , ] %>% knitr::kable()
  bias.results.s3[1, , ] %>% knitr::kable()
  bias.results.s4[1, , ] %>% knitr::kable()
  
  coef.results.s1[1, , ] %>% knitr::kable()
  coef.results.s2[1, , ] %>% knitr::kable()
  coef.results.s3[1, , ] %>% knitr::kable()
  coef.results.s4[1, , ] %>% knitr::kable()
  
  # take mean of errors across trials to find biases
  s1.bias.means <- bias.results.s1 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
  s2.bias.means <- bias.results.s2 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
  s3.bias.means <- bias.results.s3 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
  s4.bias.means <- bias.results.s4 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
  
  # take standard deviation of errors across trials to find biases
  s1.bias.sd <- bias.results.s1 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
  s2.bias.sd <- bias.results.s2 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
  s3.bias.sd <- bias.results.s3 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
  s4.bias.sd <- bias.results.s4 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
  
  colnames(s1.bias.means) <- c("Technique", "Variable", "BiasMean")
  colnames(s2.bias.means) <- c("Technique", "Variable", "BiasMean")
  colnames(s3.bias.means) <- c("Technique", "Variable", "BiasMean")
  colnames(s4.bias.means) <- c("Technique", "Variable", "BiasMean")
  
  colnames(s1.bias.sd) <- c("Technique", "Variable", "BiasSD")
  colnames(s2.bias.sd) <- c("Technique", "Variable", "BiasSD")
  colnames(s3.bias.sd) <- c("Technique", "Variable", "BiasSD")
  colnames(s4.bias.sd) <- c("Technique", "Variable", "BiasSD")
  
  # add standard deviation column to means table
  s1.bias.means$BiasSD <- s1.bias.sd$BiasSD
  s2.bias.means$BiasSD <- s2.bias.sd$BiasSD
  s3.bias.means$BiasSD <- s3.bias.sd$BiasSD
  s4.bias.means$BiasSD <- s4.bias.sd$BiasSD
  
  # print results
  message("\n\nMean Bias of each VS Technique for each Parameter estimate:")
  message(paste(c("\n\nScenario = ", 1, ", N = ", dim(bias.results.s1)[1] ), sep = ""))
  s1.bias.means %>% knitr::kable()
  message(paste(c("\n\nScenario = ", 2, ", N = ", dim(bias.results.s2)[1]), sep = ""))
  s2.bias.means %>% knitr::kable()
  message(paste(c("\n\nScenario = ", 3, ", N = ", dim(bias.results.s3)[1]), sep = ""))
  s3.bias.means %>% knitr::kable()
  message(paste(c("\n\nScenario = ", 4, ", N = ", dim(bias.results.s4)[1]), sep = ""))
  s4.bias.means %>% knitr::kable()
  
  # save results
  s1.bias.means %>% write.csv(., "../data/bias_summary_s1.csv")
  s2.bias.means %>% write.csv(., "../data/bias_summary_s2.csv")
  s3.bias.means %>% write.csv(., "../data/bias_summary_s3.csv")
  s4.bias.means %>% write.csv(., "../data/bias_summary_s4.csv")
  
  return (list(s1.bias.means, s2.bias.means, s3.bias.means, s4.bias.means))
}


# ----- Generate Plots -----

bias.plots <- function(s1.bias.means, s2.bias.means, s3.bias.means, s4.bias.means) {
  
  dim(s1.bias.means)[1] %>% print()
  dim(s1.bias.means)[2] %>% print()
  dim(s1.bias.means)[3] %>% print()
  
  t1 <- paste0( "Bias HeatMap: \nScenario = ", 1 )
  p1 <-  ggplot(s1.bias.means , aes(Technique, Variable, fill= BiasMean)) + 
      geom_tile() +
      ggtitle(t1)
  
  t2 <- paste0( "Bias HeatMap: \nScenario = ", 2 )
  p2 <-  ggplot(s2.bias.means , aes(Technique, Variable, fill= BiasMean)) + 
      geom_tile() +
      ggtitle(t2)
  
  t3 <- paste0( "Bias HeatMap: \nScenario = ", 3 )
  p3 <-  ggplot(s3.bias.means , aes(Technique, Variable, fill= BiasMean)) + 
      geom_tile() +
      ggtitle(t3)
  
  t4 <- paste0( "Bias HeatMap: \nScenario = ", 4 )
  p4<-  ggplot(s4.bias.means , aes(Technique, Variable, fill= BiasMean)) + 
      geom_tile() +
      ggtitle(t4)
  
  # set scale globally across all heat-maps
  # makes comparisons interpretable
  message("")
  set_scale_union(p1, p2, p3, p4,
                  scale = scale_fill_gradientn( colours = c("blue", "white", "red"),
                                                values  = rescale(c(-1,0,1))) )
  
  
  
  # ----- Save Plots to Disk -----
  
  print(getwd())
  png("../plots/bias_s1.png")
  print(p1)
  dev.off()
  
  png("../plots/bias_s2.png")
  print(p2)
  dev.off()
  
  png("../plots/bias_s3.png")
  print(p3)
  dev.off()
  
  png("../plots/bias_s4.png")
  print(p4)
  dev.off()
}











