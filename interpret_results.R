# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 15/06/2023
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
load("data/results_s3.rda")
load("data/results_s4.rda")


# ----- Generate Summary Statistics -----

# load R objects
results.s1 %>% dim %>% print
results.s2 %>% dim %>% print
results.s3 %>% dim %>% print
results.s4 %>% dim %>% print

# examine raw numbers
results.s1[1, , ] %>% knitr::kable()
results.s2[1, , ] %>% knitr::kable()
results.s3[1, , ] %>% knitr::kable()
results.s4[1, , ] %>% knitr::kable()

# take mean of errors across trials
s1.means <- results.s1 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s1.means) <- c("Technique", "Variable", "Bias")
s2.means <- results.s2 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s2.means) <- c("Technique", "Variable", "Bias")
s3.means <- results.s3 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s3.means) <- c("Technique", "Variable", "Bias")
s4.means <- results.s4 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s4.means) <- c("Technique", "Variable", "Bias")


# ----- Produce Mean Bias Results -----

message("\n\nMean Bias of each VS Technique for each Parameter estimate")

message(paste(c("\n\nScenario = ", 1, ", N = ", dim(results.s1)[1] ), sep = ""))
s1.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 2, ", N = ", dim(results.s2)[1]), sep = ""))
s2.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 3, ", N = ", dim(results.s3)[1]), sep = ""))
s3.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 4, ", N = ", dim(results.s4)[1]), sep = ""))
s4.means %>% knitr::kable()


# ----- Generate Plots -----

t1 <- paste0( "Bias HeatMap: \nScenario = ", 1, ", N = ", dim(results.s1)[1] )
p1 <-  ggplot(s1.means , aes(Technique, Variable, fill= Bias)) + 
       geom_tile() +
       ggtitle(t1)

t2 <- paste0( "Bias HeatMap: \nScenario = ", 2, ", N = ", dim(results.s2)[1] )
p2 <-  ggplot(s2.means , aes(Technique, Variable, fill= Bias)) + 
       geom_tile() +
       ggtitle(t2)

t3 <- paste0( "Bias HeatMap: \nScenario = ", 3, ", N = ", dim(results.s3)[1] )
p3 <-  ggplot(s3.means , aes(Technique, Variable, fill= Bias)) + 
  geom_tile() +
  ggtitle(t3)

t4 <- paste0( "Bias HeatMap: \nScenario = ", 4, ", N = ", dim(results.s4)[1] )
p4<-  ggplot(s4.means , aes(Technique, Variable, fill= Bias)) + 
  geom_tile() +
  ggtitle(t4)

# set scale globally across all heat-maps
# makes comparisons interpretable
message("")
set_scale_union(p1, p2, p3, p4,
                scale = scale_fill_gradientn( colours = c("blue", "white", "red"),
                                                      values  = rescale(c(-1,0,1))) )



# ----- Save Plots to Disk -----

png("plots/bias_s1.png")
p1
dev.off()

png("plots/bias_s2.png")
p2
dev.off()

png("plots/bias_s3.png")
p3
dev.off()

png("plots/bias_s4.png")
p4
dev.off()



# ----- Linear Regression Error Investigation -----

# TODO: swap errors for actual param estimates !!!

message("\n\nLinear Regression Parameter Estimates for each Scenario")

# take standard deviations of errors across trials
s1.sds <- results.s1 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
colnames(s1.sds) <- c("Technique", "Variable", "Bias")
s2.sds <- results.s2 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
colnames(s2.sds) <- c("Technique", "Variable", "Bias")
s3.sds <- results.s3 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
colnames(s3.sds) <- c("Technique", "Variable", "Bias")
s4.sds <- results.s4 %>% apply(., c(2, 3), sd) %>% reshape2::melt()
colnames(s4.sds) <- c("Technique", "Variable", "Bias")

# filter to just linear regression estimates
lr.s1.means <- s1.means %>% filter(Technique == "linear")
lr.s1.sds   <- s1.sds   %>% filter(Technique == "linear")
lr.s2.means <- s2.means %>% filter(Technique == "linear")
lr.s2.sds   <- s2.sds   %>% filter(Technique == "linear")
lr.s3.means <- s3.means %>% filter(Technique == "linear")
lr.s3.sds   <- s3.sds   %>% filter(Technique == "linear")
lr.s4.means <- s4.means %>% filter(Technique == "linear")
lr.s4.sds   <- s4.sds   %>% filter(Technique == "linear")

# add new column indicating scenario of origin
lr.s1.means$Scenario <- rep(1, length.out = nrow(lr.s1.means))
lr.s1.sds$Scenario   <- rep(1, length.out = nrow(lr.s1.sds))
lr.s2.means$Scenario <- rep(2, length.out = nrow(lr.s2.means))
lr.s2.sds$Scenario   <- rep(2, length.out = nrow(lr.s2.sds))
lr.s3.means$Scenario <- rep(3, length.out = nrow(lr.s3.means))
lr.s3.sds$Scenario   <- rep(3, length.out = nrow(lr.s3.sds))
lr.s4.means$Scenario <- rep(4, length.out = nrow(lr.s4.means))
lr.s4.sds$Scenario   <- rep(4, length.out = nrow(lr.s4.sds))

# stack data-frames
lr.means <- rbind(lr.s1.means, lr.s2.means, lr.s3.means, lr.s4.means)
lr.sds   <- rbind(lr.s1.sds, lr.s2.sds, lr.s3.sds, lr.s4.sds)

# results
lr.means %>% knitr::kable()
lr.sds   %>% knitr::kable()




