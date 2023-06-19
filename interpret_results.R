# ****************************************
# Variable Selection Simulation Study
# 
# Interpret Simulation Results
# 
# Emma Tarmey
#
# Started:          14/04/2023
# Most Recent Edit: 19/06/2023
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

load("data/bias_results_s1.rda")
load("data/bias_results_s2.rda")
load("data/bias_results_s3.rda")
load("data/bias_results_s4.rda")

load("data/coef_results_s1.rda")
load("data/coef_results_s2.rda")
load("data/coef_results_s3.rda")
load("data/coef_results_s4.rda")


# ----- Generate Summary Statistics -----

# load R objects
bias.results.s1 %>% dim %>% print
bias.results.s2 %>% dim %>% print
bias.results.s3 %>% dim %>% print
bias.results.s4 %>% dim %>% print

coef.results.s1 %>% dim %>% print
coef.results.s2 %>% dim %>% print
coef.results.s3 %>% dim %>% print
coef.results.s4 %>% dim %>% print


# examine raw numbers
bias.results.s1[1, , ] %>% knitr::kable()
bias.results.s2[1, , ] %>% knitr::kable()
bias.results.s3[1, , ] %>% knitr::kable()
bias.results.s4[1, , ] %>% knitr::kable()

coef.results.s1[1, , ] %>% knitr::kable()
coef.results.s2[1, , ] %>% knitr::kable()
coef.results.s3[1, , ] %>% knitr::kable()
coef.results.s4[1, , ] %>% knitr::kable()


# take mean of errors across trials
s1.means <- bias.results.s1 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s1.means) <- c("Technique", "Variable", "Bias")
s2.means <- bias.results.s2 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s2.means) <- c("Technique", "Variable", "Bias")
s3.means <- bias.results.s3 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s3.means) <- c("Technique", "Variable", "Bias")
s4.means <- bias.results.s4 %>% apply(., c(2, 3), mean) %>% reshape2::melt()
colnames(s4.means) <- c("Technique", "Variable", "Bias")


# ----- Produce Mean Bias Results -----

message("\n\nMean Bias of each VS Technique for each Parameter estimate")

message(paste(c("\n\nScenario = ", 1, ", N = ", dim(bias.results.s1)[1] ), sep = ""))
s1.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 2, ", N = ", dim(bias.results.s2)[1]), sep = ""))
s2.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 3, ", N = ", dim(bias.results.s3)[1]), sep = ""))
s3.means %>% knitr::kable()

message(paste(c("\n\nScenario = ", 4, ", N = ", dim(bias.results.s4)[1]), sep = ""))
s4.means %>% knitr::kable()


# ----- Generate Plots -----

t1 <- paste0( "Bias HeatMap: \nScenario = ", 1, ", N = ", dim(bias.results.s1)[1] )
p1 <-  ggplot(s1.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t1)

t2 <- paste0( "Bias HeatMap: \nScenario = ", 2, ", N = ", dim(bias.results.s2)[1] )
p2 <-  ggplot(s2.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t2)

t3 <- paste0( "Bias HeatMap: \nScenario = ", 3, ", N = ", dim(bias.results.s3)[1] )
p3 <-  ggplot(s3.means , aes(Technique, Variable, fill= Bias)) + 
    geom_tile() +
    ggtitle(t3)

t4 <- paste0( "Bias HeatMap: \nScenario = ", 4, ", N = ", dim(bias.results.s4)[1] )
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

message("\n\nLinear Regression Parameter Estimates for each Scenario")

# TODO: fix column labels!


# filter to just linear regression estimates
lr.s1.means <- coef.results.s1 %>% apply(., c(2, 3), mean) %>% reshape2::melt() %>% filter(Technique == "linear")
lr.s1.sds   <- coef.results.s1 %>% apply(., c(2, 3), sd)   %>% reshape2::melt() %>% filter(Technique == "linear")

lr.s2.means <- coef.results.s2 %>% apply(., c(2, 3), mean) %>% reshape2::melt() %>% filter(Technique == "linear")
lr.s2.sds   <- coef.results.s2 %>% apply(., c(2, 3), sd)   %>% reshape2::melt() %>% filter(Technique == "linear")

lr.s3.means <- coef.results.s3 %>% apply(., c(2, 3), mean) %>% reshape2::melt() %>% filter(Technique == "linear")
lr.s3.sds   <- coef.results.s3 %>% apply(., c(2, 3), sd)   %>% reshape2::melt() %>% filter(Technique == "linear")

lr.s4.means <- coef.results.s4 %>% apply(., c(2, 3), mean) %>% reshape2::melt() %>% filter(Technique == "linear")
lr.s4.sds   <- coef.results.s4 %>% apply(., c(2, 3), sd)   %>% reshape2::melt() %>% filter(Technique == "linear")


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




