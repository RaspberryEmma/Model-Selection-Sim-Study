# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 01/05/2023
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

# load R objects
results.s1 %>% dim %>% print
results.s2 %>% dim %>% print

# examine raw numbers
results.s1[1, , ] %>% knitr::kable()
results.s2[1, , ] %>% knitr::kable()

# take expectation across trials
s1.means <- results.s1 %>% apply(., c(2, 3), mean) %>% melt()
colnames(s1.means) <- c("Technique", "Variable", "Bias")
s2.means <- results.s2 %>% apply(., c(2, 3), mean) %>% melt()
colnames(s2.means) <- c("Technique", "Variable", "Bias")

# re-scale across all scenarios 
# TODO: this!

message("\n\nMean Bias of each VS Technique for each Parameter estimate")

message(paste(c("\n\nScenario = ", 1, ", N = ", dim(results.s1)[1] ), sep = ""))
s1.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 2, ", N = ", dim(results.s2)[1]), sep = ""))
s2.means %>% knitr::kable()



# ----- Generate Plots

# Heat Map
png("plots/bias_s1.png")
t <- paste0( "Bias HeatMap: \nScenario = ", 1, ", N = ", dim(results.s1)[1] )
p <-  ggplot(s1.means , aes(Technique, Variable, fill= Bias)) + 
      geom_tile() +
      scale_fill_gradientn( colours = c("blue", "white", "red"),
                            values  = rescale(c(-1,0,1))) +
      ggtitle(t)
p
dev.off()


png("plots/bias_s2.png")
t <- paste0( "Bias HeatMap: \nScenario = ", 2, ", N = ", dim(results.s2)[1] )
p <-  ggplot(s2.means , aes(Technique, Variable, fill= Bias)) + 
      geom_tile() +
      scale_fill_gradientn( colours = c("blue", "white", "red"),
                            values  = rescale(c(-1,0,1))) +
      ggtitle(t)
p
dev.off()

