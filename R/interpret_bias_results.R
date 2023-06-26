# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 26/06/2023
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

setwd("R")
source("generate_data.R")
source("plot_rescale.R")


# ----- Read Results Data from CSV -----

load("../data/bias_results_s1.rda")
load("../data/bias_results_s2.rda")
load("../data/bias_results_s3.rda")
load("../data/bias_results_s4.rda")

load("../data/coef_results_s1.rda")
load("../data/coef_results_s2.rda")
load("../data/coef_results_s3.rda")
load("../data/coef_results_s4.rda")



# ----- Generate Summary Statistics -----

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

colnames(s1.bias.means) <- c("Technique", "Variable", "Bias")
colnames(s2.bias.means) <- c("Technique", "Variable", "Bias")
colnames(s3.bias.means) <- c("Technique", "Variable", "Bias")
colnames(s4.bias.means) <- c("Technique", "Variable", "Bias")



# ----- Produce Mean Bias Results -----

message("\n\nMean Bias of each VS Technique for each Parameter estimate:")

message(paste(c("\n\nScenario = ", 1, ", N = ", dim(bias.results.s1)[1] ), sep = ""))
s1.bias.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 2, ", N = ", dim(bias.results.s2)[1]), sep = ""))
s2.bias.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 3, ", N = ", dim(bias.results.s3)[1]), sep = ""))
s3.bias.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 4, ", N = ", dim(bias.results.s4)[1]), sep = ""))
s4.bias.means %>% knitr::kable()



# ----- Generate Plots -----

t1 <- paste0( "Bias HeatMap: \nScenario = ", 1, ", N = ", dim(bias.results.s1)[1] )
p1 <-  ggplot(s1.bias.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t1)

t2 <- paste0( "Bias HeatMap: \nScenario = ", 2, ", N = ", dim(bias.results.s2)[1] )
p2 <-  ggplot(s2.bias.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t2)

t3 <- paste0( "Bias HeatMap: \nScenario = ", 3, ", N = ", dim(bias.results.s3)[1] )
p3 <-  ggplot(s3.bias.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t3)

t4 <- paste0( "Bias HeatMap: \nScenario = ", 4, ", N = ", dim(bias.results.s4)[1] )
p4<-  ggplot(s4.bias.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t4)

# set scale globally across all heat-maps
# makes comparisons interpretable
message("")
set_scale_union(p1, p2, p3, p4,
                scale = scale_fill_gradientn( colours = c("blue", "white", "red"),
                                              values  = rescale(c(-1,0,1))) )



# ----- Save Plots to Disk -----

png("../plots/bias_s1.png")
p1
dev.off()

png("../plots/bias_s2.png")
p2
dev.off()

png("../plots/bias_s3.png")
p3
dev.off()

png("../plots/bias_s4.png")
p4
dev.off()











