# ****************************************
# Variable Selection Simulation Study
# 
# Explore Models under Study
# 
# Emma Tarmey
#
# Started:          11/04/2023
# Most Recent Edit: 12/12/2023
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


# ----- Any given scenario -----

scenario.plots <- function(s = NULL) {
  # open png file
  png(paste("../plots/synthetic_data_s", s, "_DAG.png", sep = ""))
  assign("p", do.call(paste("draw.DAG.", s, sep = ""), list()))
  p
  
  # close png file
  dev.off()
  
  # data - scenario - trial
  assign("data.s.1", do.call(paste("generate.data.", s, sep = ""), list()))
  assign("data.s.2", do.call(paste("generate.data.", s, sep = ""), list()))
  assign("data.s.3", do.call(paste("generate.data.", s, sep = ""), list()))
  
  data.s.1 %>% head() %>% knitr::kable()
  data.s.2 %>% head() %>% knitr::kable()
  data.s.3 %>% head() %>% knitr::kable()
  
  data.s.1 %>% cor()
  data.s.2 %>% cor()
  data.s.3 %>% cor()
  
  # Correlation plot
  png( paste("../plots/synthetic_data_s", s, "_t1_corr.png", sep = "") )
  p <- data.s.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
    ggtitle( paste("Synthetic Data Correlation Plot - Scenario ", s, " - Trial 1", sep = "") )
  p
  dev.off()
  
  png( paste("../plots/synthetic_data_s", s, "_t2_corr.png", sep = "") )
  p <- data.s.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
    ggtitle( paste("Synthetic Data Correlation Plot - Scenario ", s, " - Trial 2", sep = "") )
  p
  dev.off()
  
  png( paste("../plots/synthetic_data_s", s, "_t3_corr.png", sep = "") )
  p <- data.s.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
    ggtitle( paste("Synthetic Data Correlation Plot - Scenario ", s, " - Trial 3", sep = "") )
  p
  dev.off()
}



