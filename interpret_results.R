# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 04/05/2023
# ****************************************


# ----- Preamble -----

rm(list = ls())

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

source("plot_rescale.R")


# ----- Read Results Data from CSV -----

load("data/results_s1.rda")
load("data/results_s2.rda")


# ----- Examine Results -----

# load R objects
results.s1 %>% dim %>% print
results.s2 %>% dim %>% print

# examine raw numbers
results.s1[1, , ] %>% knitr::kable()
results.s2[1, , ] %>% knitr::kable()

# take expectation across trials
s1.means <- results.s1 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s1.means) <- c("Technique", "Variable", "Bias")
s2.means <- results.s2 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s2.means) <- c("Technique", "Variable", "Bias")

message("\n\nMean Bias of each VS Technique for each Parameter estimate")

message(paste(c("\n\nScenario = ", 1, ", N = ", dim(results.s1)[1] ), sep = ""))
s1.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 2, ", N = ", dim(results.s2)[1]), sep = ""))
s2.means %>% knitr::kable()



# ----- Generate Plots -----

t1 <- paste0( "Bias HeatMap: \nScenario = ", 1, ", N = ", dim(results.s1)[1] )
p1 <-  ggplot(s1.means , aes(Technique, Variable, fill= Bias)) + 
       geom_tile() +
       ggtitle(t1)

t2 <- paste0( "Bias HeatMap: \nScenario = ", 2, ", N = ", dim(results.s2)[1] )
p2 <-  ggplot(s2.means , aes(Technique, Variable, fill= Bias)) + 
       geom_tile() +
       ggtitle(t2)

# set scale globally across all heat-maps
# makes comparisons interpretable
message("")
set_scale_union(p1, p2,
                scale = scale_fill_gradientn( colours = c("blue", "white", "red"),
                                                      values  = rescale(c(-1,0,1))) )



# ----- Save Plots to Disk -----

png("plots/bias_s1.png")
p1
dev.off()

png("plots/bias_s2.png")
p2
dev.off()
